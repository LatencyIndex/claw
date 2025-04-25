module Claw.Audio (
    -- | Audio editing and conversion utilities. Requires @ffmpeg@.
    toMp3,
) where

import Claw.FilePath
import System.Process (callProcess)
import System.Directory (copyFile)

-- | Convert to destination file, with type determined by extension.
ffconvert :: FilePath -> FilePath -> IO ()
ffconvert src_file dst_file = callProcess "ffmpeg" ["-i", src_file, dst_file]

-- | Convert file to mp3 to destination directory.
-- If src_file is @src_dir/file.flac@, destination file will be @dst_dir/file.mp3@
-- Returns the destination filepath.
toMp3 :: FilePath -> FilePath -> IO FilePath
toMp3 dst_dir src_file =
    let dst_file = dst_dir </> getBaseName src_file <.> "mp3"
    in do
        if getExt src_file == ".mp3"
        then copyFile src_file dst_file
        else ffconvert src_file dst_file
        return dst_file
