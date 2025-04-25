{-# LANGUAGE FlexibleInstances #-}

module Claw.FileSystem (
    INode (..),
    cd,
    getINode,
    ll,
    ls,
    lsdir,
    modifyFile,
    pwd,
    renameFile,
    trash,
) where

import Claw.FilePath
import Claw.Internal.List (align, padL, padR)
import Claw.Internal.PrettyPrint (Pretty, pprint)
import Claw.Time (UTCTime, getCurrentTimeZone, showTime, utcToZonedTime)
import Data.Functor ((<&>))
import Data.List (sort, transpose)
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Numeric (showFFloat)
import System.Directory (renameFile)
import System.Process (callProcess)
import qualified System.Directory as D
import qualified System.FilePath as F
import qualified System.IO as I
import qualified System.Posix.Files as P

-- | Express the size of data in the most appropriate unit of bytes, as a (quantity, unit) pair.
inXBytes :: Int -> (Float, String)
inXBytes n = (ratio, prefix)
  where
    binarySi =
        [ (2 ^ (10 :: Int), "kB"),
          (2 ^ (20 :: Int), "MB"),
          (2 ^ (30 :: Int), "GB"),
          (2 ^ (40 :: Int), "TB"),
          (2 ^ (50 :: Int), "PB")
        ]
    canFit (qty, _siPrefix) = qty <= abs n
    (unit, prefix) = last $ (1, "B") : takeWhile canFit binarySi
    ratio = fromIntegral n / fromIntegral unit :: Float

-- | A filesystem entry, like a directory, file, link, block device, etc.
data INode = INode
    { -- | Only the name itself, without the path.
      name :: String,
      size :: Int,
      modified :: UTCTime,
      is_regular_file :: Bool,
      is_dir :: Bool,
      is_symlink :: Bool
    }
    deriving (Eq, Show)

instance Ord INode where
    compare = comparing (not . is_dir) <> comparing (getExt . name) <> comparing name

instance Pretty INode where
    pprint x = pprint [x]
instance Pretty [INode] where
    pprint xs = do
        timezone <- getCurrentTimeZone
        let
            modCell :: INode -> String
            modCell = showTime . utcToZonedTime timezone . modified
            sizeCell :: INode -> (String, String)
            sizeCell x
                | is_regular_file x = (showFFloat (Just 2) qty "", unit)
                | otherwise = ("", "")
              where
                (qty, unit) = inXBytes (size x)
            dirCell :: INode -> String
            dirCell x
                | is_regular_file x = "file"
                | is_dir x = "dir"
                | otherwise = "?"
            linkCell :: INode -> String
            linkCell x = if is_symlink x then "symlink" else ""
            extCell :: INode -> String
            extCell x = if is_regular_file x then getExt (name x) else ""
            alignR :: [String] -> [String]
            alignR = align (padL ' ')
            alignL :: [String] -> [String]
            alignL = align (padR ' ')

            modCol = alignL $ modCell <$> xs
            sizeCol = sizeCell <$> xs
            sizeQtyCol = alignR $ fst <$> sizeCol
            sizeUnitCol = alignL $ snd <$> sizeCol
            dirCol = alignL $ dirCell <$> xs
            linkCol = alignL $ linkCell <$> xs
            extCol = alignL $ extCell <$> xs
            nameCol = alignL $ name <$> xs

            cols = [modCol, sizeQtyCol, sizeUnitCol, dirCol, linkCol, extCol, nameCol]
            rows = transpose cols
        putStr $ unlines $ unwords <$> rows

getINode :: FilePath -> IO INode
getINode file = do
    status <- P.getFileStatus file
    return
        INode
            { name = getFileName file,
              size = fromIntegral $ P.fileSize status,
              modified = posixSecondsToUTCTime $ realToFrac $ P.modificationTime status,
              is_regular_file = P.isRegularFile status,
              is_dir = P.isDirectory status,
              is_symlink = P.isSymbolicLink status
            }

-- | Names of all entries in the working directory, without the special entries @.@ and @..@
-- Entires given relative to the working directory, i.e. only their filenames are returned.
ls :: IO [FilePath]
ls = pwd >>= D.listDirectory

-- | Names of all entries in the given directory, without the special entries @.@ and @..@
-- Entires given relative to the same directory that dir is relative to.
lsdir :: FilePath -> IO [FilePath]
lsdir dir = D.listDirectory dir <&> fmap (dir </>)

-- | All entries in the working directory, without the special entries @.@ and @..@
-- Entries are sorted with directories first, then by file type, then by name.
ll :: IO [INode]
ll = ls >>= mapM getINode <&> sort

-- | Obtain the current working directory as an absolute path.
pwd :: IO FilePath
pwd = D.getCurrentDirectory

-- | Change the working directory to the given path.
cd :: FilePath -> IO ()
cd = D.setCurrentDirectory

-- | Uses a temporary file to avoid data loss.
modifyFile :: (String -> String) -> FilePath -> IO ()
modifyFile f file = do
    -- Make a temporary file
    let dir = getDir file
        fname = getFileName file
    (tempName, tempHandle) <- I.openTempFile dir (fname F.<.> "clawtmp")
    -- Read target file contents
    handle <- I.openFile file I.ReadMode
    contents <- I.hGetContents handle
    -- Write modified contents to temp file
    let newContents = f contents
    I.hPutStr tempHandle newContents
    -- Close both files
    I.hClose handle
    I.hClose tempHandle
    -- Replace original file with modified version
    D.removeFile file
    D.renameFile tempName file

-- | Move file or directory to trash.
trash :: FilePath -> IO ()
trash path = callProcess "trash" [path]
