module Claw.FS (
    EpochTime,
    INode (..),
    cd,
    getInode,
    ll,
    ls,
    modifyFile,
    pwd,
    renameFile,
) where

import Claw.FilePath
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Ord (comparing)
import System.Directory (renameFile)
import qualified System.Directory as D
import qualified System.FilePath as F
import qualified System.IO as I
import qualified System.Posix.Files as P
import System.Posix.Types (EpochTime)

-- | A filesystem entry, like a directory, file, link, block device, etc.
data INode = INode
    { -- | Only the name itself, without the path.
      name :: String,
      size :: Int,
      modified :: EpochTime,
      is_regular_file :: Bool,
      is_dir :: Bool,
      is_symlink :: Bool
    }
    deriving (Eq, Show)

instance Ord INode where
    compare = comparing is_dir <> comparing (getExt . name) <> comparing name

getInode :: FilePath -> IO INode
getInode file = do
    status <- P.getFileStatus file
    return
        INode
            { name = getFileName file,
              size = fromIntegral (P.fileSize status),
              modified = P.modificationTime status,
              is_regular_file = P.isRegularFile status,
              is_dir = P.isDirectory status,
              is_symlink = P.isSymbolicLink status
            }

-- | Names of all entries in the working directory, without the special entries @.@ and @..@
-- Entires given relative to the working directory, i.e. only their filenames are returned.
ls :: IO [FilePath]
ls = pwd >>= D.listDirectory

-- | All entries in the working directory, without the special entries @.@ and @..@
-- Entries are sorted with directories first, then by file type, then by name.
ll :: IO [INode]
ll = ls >>= mapM getInode <&> sort

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
