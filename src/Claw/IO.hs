module Claw.IO (
    ls,
    pwd,
    cd,
    modifyFile,
    renameFile,
) where

import Claw.FilePath
import System.Directory (renameFile)
import qualified System.Directory as D
import qualified System.FilePath as F
import qualified System.IO as I

{- | Names of all entries in the working directory, without the special entries @.@ and @..@
Entires given relative to the working directory, i.e. only their filenames are returned.
-}
ls :: IO [FilePath]
ls = pwd >>= D.listDirectory

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
        name = getFileName file
    (tempName, tempHandle) <- I.openTempFile dir (name F.<.> "clawtmp")
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
