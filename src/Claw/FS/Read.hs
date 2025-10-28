{-# LANGUAGE FlexibleInstances #-}

module Claw.FS.Read (
    Metadata (..),
    cd,
    getMetadata,
    lsdir,
    ls,
    lsmeta,
    pwd,
    findUnique,
    doesFileExist,
    doesPathExist,
    doesDirectoryExist,
    isRegularFile,
    getFileSize,
) where

import Claw.FS.Path
import Claw.Internal.List (align, padL, padR)
import Claw.Internal.PrettyPrint (Pretty, pprint)
import Claw.Time (UTCTime, getCurrentTimeZone, showTime, utcToZonedTime)
import Control.Exception (SomeException, try)
import Control.Exception.Extra (try_)
import Control.Monad.Extra (findM, ifM)
import Data.Either (fromRight, rights)
import Data.Either.Extra (eitherToMaybe)
import Data.Functor ((<&>))
import Data.List (sort, transpose)
import Data.Ord (comparing)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Numeric (showFFloat)
import System.Directory (doesDirectoryExist, doesFileExist, doesPathExist)
import qualified System.Directory as D
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
data Metadata = Metadata
    { -- | Only the name itself, without the path.
      name :: String,
      size :: Int,
      modified :: UTCTime,
      -- | False for directories and symlinks.
      is_regular_file :: Bool,
      -- | False for symlinks to directories.
      is_dir :: Bool,
      is_symlink :: Bool
    }
    deriving (Eq, Show)

instance Ord Metadata where
    compare = comparing (not . is_dir) <> comparing (getExt . name) <> comparing name

instance Pretty Metadata where
    pprint x = pprint [x]
instance Pretty [Metadata] where
    pprint xs = do
        timezone <- getCurrentTimeZone
        let
            modCell :: Metadata -> String
            modCell = showTime . utcToZonedTime timezone . modified
            sizeCell :: Metadata -> (String, String)
            sizeCell x
                | is_regular_file x = (showFFloat (Just 2) qty "", unit)
                | otherwise = ("", "")
              where
                (qty, unit) = inXBytes (size x)
            dirCell :: Metadata -> String
            dirCell x
                | is_regular_file x = "file"
                | is_dir x = "dir"
                | otherwise = "?"
            linkCell :: Metadata -> String
            linkCell x = if is_symlink x then "symlink" else ""
            extCell :: Metadata -> String
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

-- | Warning: For information about a directory itself, use @dirname@, not @dirname/@
getMetadata :: FilePath -> IO (Either SomeException Metadata)
getMetadata filepath = try (P.getSymbolicLinkStatus filepath) <&> fmap mkMetadata
  where
    mkMetadata :: P.FileStatus -> Metadata
    mkMetadata status =
        Metadata
            { name = getFileName filepath,
              size = fromIntegral $ P.fileSize status,
              modified = posixSecondsToUTCTime $ realToFrac $ P.modificationTime status,
              is_regular_file = P.isRegularFile status,
              is_dir = P.isDirectory status,
              is_symlink = P.isSymbolicLink status
            }

-- | Names of all entries in the given directory, without the special entries @.@ and @..@
-- Entries given relative to the same directory that dir is relative to.
-- If filepath does not exist returns the empty list.
lsdir :: FilePath -> IO [FilePath]
lsdir dir = try_ (D.listDirectory dir) <&> fmap (dir </>) . fromRight []

-- | Names of all entries in the working directory, without the special entries @.@ and @..@
-- Entries given relative to the working directory, i.e. only their filenames are returned.
ls :: IO [FilePath]
ls = pwd >>= D.listDirectory

-- | All entries in the working directory, without the special entries @.@ and @..@
-- Entries are sorted with directories first, then by file type, then by name.
lsmeta :: IO [Metadata]
lsmeta = ls >>= mapM getMetadata <&> (sort . rights)

-- | Obtain the current working directory as an absolute path.
pwd :: IO FilePath
pwd = D.getCurrentDirectory

-- | Change the working directory to the given path.
-- Throws if directory does not exist.
cd :: FilePath -> IO ()
cd = D.setCurrentDirectory

-- | File exists and is a regular file, not a directory or symlink.
isRegularFile :: FilePath -> IO Bool
isRegularFile path = do
    e_mdata <- getMetadata path
    return $ case e_mdata of
        Left _ -> False
        Right metadata -> is_regular_file metadata

-- | In bytes. @Nothing@ if path is not a regular file.
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize file = ifM (isRegularFile file) fileSize (return Nothing)
  where
    -- Get file size without throwing exception.
    fileSize :: IO (Maybe Integer)
    fileSize = eitherToMaybe <$> try_ (D.getFileSize file)

-- | Appends numbers to the end of the filename until it is unique.
-- E.g. given @file.jpg@, it will try @file.jpg@, @file0.jpg@, @file1.jpg@,.. up to @file999.jpg@
findUnique :: FilePath -> IO (Maybe FilePath)
findUnique file =
    let
        prefix = getDir file </> getBaseName file
        suffixes = "" : (show <$> [0 :: Int .. 999])
        ext = getExt file
        candidates :: [FilePath]
        candidates = (\suffix -> prefix ++ suffix <.> ext) <$> suffixes
     in
        findM (fmap not . doesPathExist) candidates
