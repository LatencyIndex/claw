module Claw.Internal.Conversion (
    convertInPlace,
    convertSafely,
) where

import Claw.FS.Read (doesPathExist, isRegularFile)
import Claw.FS.Write (trash)
import Control.Exception.Extra (ignore)
import Data.Functor ((<&>))

-- | Converts a file, and deletes the obsolete version. Arguments:
--      conversionFn: takes the original file, and returns the converted file. Must conform to convertSafely behavior.
--      findObsolteFn: takes the original and converted file, and returns the one that should be discarded
--      original: file to be converted
convertInPlace ::
    (FilePath -> IO (Maybe FilePath)) ->
    (FilePath -> FilePath -> IO FilePath) ->
    FilePath ->
    IO ()
convertInPlace conversionFn findObsoleteFn original = do
    mconverted <- conversionFn original
    case mconverted of
        Nothing -> return ()
        Just converted -> findObsoleteFn original converted >>= trash

-- | Given a conversion function that takes a src and dst path and tries to convert src to dst,
-- yields a new conversion function, that checks that src is a regular file (not a directory or symlink),
-- that src /= dst, and that dst does not yet exist, before attempting to convert.
-- Ignores any exceptions from the conversion function.
-- If the conversion was successful (i.e. a new file was created), returns the path to the new file.
convertSafely :: (FilePath -> FilePath -> IO ()) -> FilePath -> FilePath -> IO (Maybe FilePath)
convertSafely convertFn src dst = do
    src_exists <- isRegularFile src
    dst_taken <- doesPathExist dst
    if src == dst || not src_exists || dst_taken
        then
            return Nothing
        else do
            -- Ignore all exceptions, such as caused by e.g. wrong filenames.
            ignore (convertFn src dst)
            doesPathExist dst <&> \exists -> if exists then Just dst else Nothing
