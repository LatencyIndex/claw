module Claw.Multimedia (
    compressDestructive,
    compressRecursive,
    isWebsiteDir,
    isCompressibleDir,
    listCompressible,
) where

import Claw.FS.Path (getExt)
import Claw.FS.Read (isRegularFile, isSymlink', lsDir)
import Claw.Image (countFrames, toWebpDestructive)
import Claw.Video (toMp4Destructive)
import Control.Monad (filterM, (>=>))
import Data.Char (toLower)
import Data.List (isSuffixOf)
import System.Directory.Extra (listFilesInside)

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
            _ -> return ()
    _ -> return ()

-- | Apply compressDestructive to the files in the given folder and all subfolders
-- Does not follow or try to compress symlinks, and skips directories that represent saved websites.
compressRecursive :: FilePath -> IO ()
compressRecursive = listCompressible >=> compressMany

compressMany :: [FilePath] -> IO ()
compressMany files = mapM_ compressVerbose (zip [1 ..] files)
  where
    n = length files
    compressVerbose :: (Int, FilePath) -> IO ()
    compressVerbose (i, path) = do
        putStrLn $ "compressing " ++ show i ++ "/" ++ show n ++ ": " ++ path
        compressDestructive path

-- | Directory represents a website stored for offline use.
isWebsiteDir :: FilePath -> IO Bool
isWebsiteDir dir = do
    files <- lsDir dir
    return $ (("_files" `isSuffixOf` dir) || ("_files/" `isSuffixOf` dir)) && any isWebFile files
  where
    isWebFile :: FilePath -> Bool
    isWebFile = (`elem` [".css", ".js", ".html", ".htm"]) . getExt

-- | Path is neither a symlink nor a directory with website files.
-- Does not check if path is a directory, because @listFilesInside@ only calls it on directories.
isCompressibleDir :: FilePath -> IO Bool
isCompressibleDir dir = do
    link <- isSymlink' dir
    website <- isWebsiteDir dir
    return (not link && not website)

isCompressibleFile :: FilePath -> IO Bool
isCompressibleFile path = (rightFiletype &&) <$> isRegularFile path
  where
    rightFiletype = (toLower <$> getExt path) `elem` [".png", ".jpg", ".jpeg", ".bmp", ".gif"]

-- | All files that can be safely compressed. I.e. compressible files in compressible directories.
listCompressible :: FilePath -> IO [FilePath]
listCompressible = listFilesInside isCompressibleDir >=> filterM isCompressibleFile
