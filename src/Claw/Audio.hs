-- | Audio editing and conversion utilities. Requires @ffmpeg@.
module Claw.Audio (
    toMp3,
    toMp3HQDestructive,
) where

import Claw.FS.Path
import Claw.FS.Read (findUnique)
import Claw.Internal.Conversion (findObsoleteBySize, convertDestructive)
import System.Directory (copyFile)
import System.Process (callProcess)

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

-- | Given an original and converted file, return the one that is obsolete.
-- If the conversion failed, that should be the converted file, otherwise the original file.
mp3FindObsolete :: FilePath -> FilePath -> IO (Maybe FilePath)
mp3FindObsolete = findObsoleteBySize minSizeAbs maxSizeRel where
    minSizeAbs = 2048 -- Audio smaller than 2kB is probably broken.
    maxSizeRel = 0.9

-- | Convert to high-quality variable bit-rate .mp3. Destination file should have .mp3 extension.
convertMp3HQ :: FilePath -> FilePath -> IO ()
convertMp3HQ src_file dst_file = callProcess "ffmpeg" [
        "-i", src_file,
        "-c:a", "libmp3lame", -- use LAME mp3 encoder
        "-q:a", "0", -- LAME's highest quality variable bit rate setting
        dst_file
    ]

-- | Convert to high-quality variable bit-rate .mp3, returning the path to the converted file on success.
toMp3HQ :: FilePath -> IO (Maybe FilePath)
toMp3HQ src = do
    m_dst <- findUnique (replaceExt ".mp3" src) -- Find free destination filename.
    case m_dst of
        Nothing -> return Nothing -- No free filename found - give up.
        Just dst -> convertMp3HQ src dst >> return (Just dst)

-- | Convert to highest-quality variable bitrate .mp3, trashing the original file on success.
toMp3HQDestructive :: FilePath -> IO ()
toMp3HQDestructive = convertDestructive toMp3HQ mp3FindObsolete
