module Claw.Internal.Process (
    callProcessSafe,
    readProcessSafe,
) where

import Control.Exception.Extra (ignore)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (callProcess, readProcessWithExitCode)

-- | Like callProcess, but ignores exceptions.
callProcessSafe :: FilePath -> [String] -> IO ()
callProcessSafe exe args = ignore (callProcess exe args)

-- | Like readProcessWithExitCode, but returns @Just stdout@ on success, and @Nothing@ on failure.
readProcessSafe :: FilePath -> [String] -> String -> IO (Maybe String)
readProcessSafe exe args stdin = onlySuccess <$> readProcessWithExitCode exe args stdin
  where
    onlySuccess :: (ExitCode, String, String) -> Maybe String
    onlySuccess (ExitSuccess, stdout, _stderr) = Just stdout
    onlySuccess _ = Nothing
