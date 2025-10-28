-- | Higher-order functions for file conversion.
module Claw.Internal.Conversion (
    convertDestructive,
    convertSafely,
    findObsoleteBySize,
) where

import Claw.FS.Read (doesPathExist, getFileSize, isRegularFile)
import Claw.FS.Write (trash)
import Control.Exception.Extra (ignore)
import Data.Functor ((<&>))

-- | Converts a file, and deletes the obsolete version. Arguments:
--      conversionFn: takes the original file, and returns the converted file. Must conform to convertSafely behavior.
--      findObsolteFn: takes the original and converted file, and returns the one that should be discarded, if any
--      original: file to be converted
convertDestructive ::
    (FilePath -> IO (Maybe FilePath)) ->
    (FilePath -> FilePath -> IO (Maybe FilePath)) ->
    FilePath ->
    IO ()
convertDestructive conversionFn findObsoleteFn original = do
    mconverted <- conversionFn original
    case mconverted of
        Nothing -> return ()
        Just converted -> findObsoleteFn original converted >>= trashMaybe

trashMaybe :: Maybe FilePath -> IO ()
trashMaybe = maybe (pure ()) trash

-- | Given a conversion function that takes a src and dst path and tries to convert src to dst,
-- yields a new conversion function, that checks that src is a regular file (not a directory or symlink),
-- that src /= dst, and that dst does not yet exist, before attempting to convert.
-- Ignores any exceptions from the conversion function.
-- If the conversion was successful (i.e. a new file was created), returns the path to the new file.
convertSafely :: (FilePath -> FilePath -> IO ()) -> FilePath -> FilePath -> IO (Maybe FilePath)
convertSafely convertFn src dst = do
    src_exists <- isRegularFile src
    dst_taken <- doesPathExist dst
    if src /= dst && src_exists && not dst_taken
        then do
            -- Ignore all exceptions, such as caused by e.g. wrong filenames.
            ignore (convertFn src dst)
            doesPathExist dst <&> \exists -> if exists then Just dst else Nothing
        else
            return Nothing

-- | Build a function to determine which of two files, original or converted, to discard. Arguments:
--      minSizeAbs: converted files smaller than this many bytes are considered broken
--      maxSizeRel: given the extension of the source file (e.g. @.jpg@), returns the maximum size of the converted file
--                  relative to the original, before it is considered too big
findObsoleteBySize :: Integer -> (String -> Float) -> FilePath -> FilePath -> IO (Maybe FilePath)
findObsoleteBySize minSizeAbs maxSizeRel original converted = do
    m_orig_size <- getFileSize original
    m_conv_size <- getFileSize converted
    return $ case (m_orig_size, m_conv_size) of
        (Just orig_size, Just conv_size) -> Just (if small_enough && big_enough then original else converted)
          where
            max_size = maxSizeRel original * fromInteger orig_size
            small_enough = fromInteger conv_size <= max_size
            big_enough = minSizeAbs < conv_size
        _ -> Nothing
