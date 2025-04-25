module Claw.Audio (
    -- | Audio editing and conversion utilities. Requires @ffmpeg@.
    toMp3,
) where

import Claw.FilePath
import System.Process (callProcess)
import System.Directory (copyFile)

-- | Convert to destination file, with type determined by extension.
-- If source and destination types are the same, skip conversion and just copy.
ffconvert :: FilePath -> FilePath -> IO ()
ffconvert src_file dst_file =
    if getExt src_file == getExt dst_file
    then copyFile src_file dst_file
    else callProcess "ffmpeg" ["-i", src_file, dst_file]

-- | Convert file to mp3 to destination directory.
-- If src_file is @src_dir/file.flac@, destination file will be @dst_dir/file.mp3@
-- Returns the destination filepath.
toMp3 :: FilePath -> FilePath -> IO FilePath
toMp3 dst_dir src_file =
    let dst_file = dst_dir </> getBaseName src_file <.> "mp3"
    in ffconvert src_file dst_file >> return dst_file
