module Claw.FilePath (
    getFileName,
    getBaseName,
    getDir,
    getExt,
    hasExt,
    replaceExt,
    (</>),
    (<.>),
) where

import System.FilePath (
    (<.>),
    (</>),
 )
import qualified System.FilePath as F

{- | Get the file name.

> takeFileName "/directory/file.ext" == "file.ext"
> takeFileName "test/" == ""
> isSuffixOf (takeFileName x) x
> takeFileName x == snd (splitFileName x)
> Valid x => takeFileName (replaceFileName x "fred") == "fred"
> Valid x => takeFileName (x </> "fred") == "fred"
> Valid x => isRelative (takeFileName x)
-}
getFileName :: FilePath -> FilePath
getFileName = F.takeFileName

{- | Get the base name, without an extension or path.

> baseName "/directory/file.ext" == "file"
> baseName "file/test.txt" == "test"
> baseName "dave.ext" == "dave"
> baseName "" == ""
> baseName "test" == "test"
> baseName "file/file.tar.gz" == "file.tar"
-}
getBaseName :: FilePath -> String
getBaseName = F.takeBaseName

{- | Get the directory name, move up one level.

>           takeDirectory "/directory/other.ext" == "/directory"
>           isPrefixOf (takeDirectory x) x || takeDirectory x == "."
>           takeDirectory "foo" == "."
>           takeDirectory "/" == "/"
>           takeDirectory "/foo" == "/"
>           takeDirectory "/foo/bar/baz" == "/foo/bar"
>           takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
>           takeDirectory "foo/bar/baz" == "foo/bar"
> Windows:  takeDirectory "foo\\bar" == "foo"
> Windows:  takeDirectory "foo\\bar\\\\" == "foo\\bar"
> Windows:  takeDirectory "C:\\" == "C:\\"
-}
getDir :: FilePath -> FilePath
getDir = F.takeDirectory

{- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.

> extension "/directory/path.ext" == ".ext"
> extension "/directory/path.tar.gz" == ".gz"
-}
getExt :: FilePath -> String
getExt = F.takeExtension

{- | Whether the file has the specified extension.

> hasExt ".zip" "file.zip" == True
> hasExt "zip" "file.zip" == False
-}
hasExt :: String -> FilePath -> Bool
hasExt ext file = ext == getExt file

{- | Set the extension of a file, overwriting one if already present.

> replaceExt "ext" "/directory/path.txt" == "/directory/path.ext"
> replaceExt ".ext" "/directory/path.txt" == "/directory/path.ext"
> replaceExt ".bob" "file.txt" == "file.bob"
> replaceExt "bob" "file.txt" == "file.bob"
> replaceExt ".bob" "file" == "file.bob"
> replaceExt "" "file.txt" == "file"
> replaceExt "txt" "file.fred.bob" == "file.fred.txt"
-}
replaceExt :: String -> FilePath -> FilePath
replaceExt ext file = F.replaceExtension file ext
