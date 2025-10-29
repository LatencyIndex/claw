module Claw.Multimedia (compressDestructive, compressRecursive) where

import Claw.FS.Path (getExt)
import Claw.FS.Read (treeOrdinary)
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

-- | Apply compressDestructive to the files in the given folder and all subfolders
-- Does not follow or try to compress symlinks.
compressRecursive :: FilePath -> IO ()
compressRecursive dir = do
    files <- zip [1 ..] . filter isCompressible <$> treeOrdinary dir
    let n = length files
        compressVerbose :: (Int, FilePath) -> IO ()
        compressVerbose (i, path) = do
            putStrLn $ "compressing " ++ show i ++ "/" ++ show n ++ ": " ++ path
            compressDestructive path
    mapM_ compressVerbose files
  where
    isCompressible :: FilePath -> Bool
    isCompressible = (`elem` [".png", ".jpg", ".jpeg", ".bmp", ".gif"]) . (toLower <$>) . getExt
