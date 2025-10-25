module Claw.FS.Write (
    modifyFile,
    renameFile,
    trash,
    cp,
) where

import Claw.FS.Path
import Control.Exception.Extra (ignore)
import System.Directory (renameFile)
import qualified System.Directory as D
import qualified System.FilePath as F
import qualified System.IO as I
import System.Process (callProcess)

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
-- Ignores exceptions, e.g. caused by invalid paths.
trash :: FilePath -> IO ()
trash path = ignore $ callProcess "trash" [path]

cp :: FilePath -> FilePath -> IO ()
cp = D.copyFile
