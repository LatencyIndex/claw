module Main (main) where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Process (callProcess)

main :: IO ()
main = do
    home <- getHomeDirectory
    let init_script = home </> ".config/claw/init.ghci"
    -- Unlike `ghci`, `cabal repl` will automatically load packages that claw depends on.
    callProcess
        "cabal"
        [ -- https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-repl
          "repl",
          "--ignore-project", -- Ignore any projects/.cabal files that happen to be in current directory.
          -- Build packages and make them available to the repl:
          --    To depend on multiple packages (-b is an alias for --build-depends):
          --    > cabal repl -bfirstpkg -bsecondpkg -bthirdpkg
          --    Version constraints can be given just like in a .cabal file:
          --    > cabal repl -b'claw >= 0.0.1.0'
          "--build-depends",
          "claw",
          "--ghc-options", -- Supposedly replaced by repl-options, but doesn't work as-is.
          "-ignore-dot-ghci", -- Don’t read either ./.ghci or the other startup files when starting up.
          "--ghc-options",
          "-ghci-script \"" ++ init_script ++ "\"", -- Run this script at repl startup.
          "--ghc-options",
          "-interactive-print=Claw.Utils.PrettyPrint.pprint" -- Function to use for printing values
        ]
