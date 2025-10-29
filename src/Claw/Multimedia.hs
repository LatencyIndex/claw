module Claw.Multimedia (compressDestructive) where

import Claw.FS.Path (getExt)
import Claw.Image (countFrames, toWebpDestructive)
import Claw.Video (toMp4Destructive)
import Data.Char (toLower)

-- | Destructively convert an image to a smaller format, .webp or .mp4 for animations.
-- On success, trashes the original file.
compressDestructive :: FilePath -> IO ()
compressDestructive path = case toLower <$> getExt path of
    ".png" -> toWebpDestructive path
    ".jpg" -> toWebpDestructive path
    ".jpeg" -> toWebpDestructive path
    ".bmp" -> toWebpDestructive path
    ".gif" -> do
        nb_frames <- countFrames path
        case nb_frames of
            Just n
                | n == 1 -> toWebpDestructive path -- static image in gif format
                -- gifs with very few frames are probably slideshows, so don't convert them.
                | n > 20 -> toMp4Destructive 24 path
                | otherwise -> return ()
            Nothing -> return ()
    _ -> return ()
