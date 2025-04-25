module Claw.IO (
    ls,
    lsDir,
    pwd,
    cd,
    modifyFile,
    readFile',
    renameFile,
) where

import Claw.Control
import System.Directory (renameFile)
import qualified System.Directory as D
import System.IO (readFile')

{- | Names of all entries in the working directory, without the special entries @.@ and @..@
Entires given relative to the working directory, i.e. only their filenames are returned.
-}
ls :: IO [FilePath]
ls = pwd >>= D.listDirectory

-- | Absolute paths to all entries in the directory, without the special entries @.@ and @..@
lsDir :: FilePath -> IO [FilePath]
lsDir = mapM D.makeAbsolute <=< D.listDirectory

-- | Obtain the current working directory as an absolute path.
pwd :: IO FilePath
pwd = D.getCurrentDirectory

-- | Change the working directory to the given path.
cd :: FilePath -> IO ()
cd = D.setCurrentDirectory

-- | Modify a file in-place.
-- TODO use a temporary file and rename instead, for safety
modifyFile :: (String -> String) -> FilePath -> IO ()
-- Use the non-lazy readFile', otherwise the file remains open and locked for writing.
modifyFile f file = readFile' file <&> f >>= writeFile file
