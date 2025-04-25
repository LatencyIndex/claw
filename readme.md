# About

Ad-hoc library to use Haskell as a shell replacement, with convenience functions for common tasks. In a pre-alpha state, updated sporadically. However, it should be perfectly usable, since it's only a library on top of the Haskell REPL.

# Installation

Create [local repository][https://cabal.readthedocs.io/en/stable/config.html#local-no-index-repositories] (named 'lepository' in this example):
    `mkdir "$HOME/.cabal/lepository"`
    Add following lines to cabal config file (usually `~/.cabal/config`)
        `repository lepository`
        `    url: file+noindex:///home/your_username/.cabal/lepository`
    To use library in a project, specify it as a dependency in `yourproject.cabal`:
        `build-depends: claw`
Install library and executable (script assumes the 'lepository' name)
    `install.sh`
Build documentation (optional)
    `cabal haddock`

# Cleanup

Old packages accumulate in `~/.cabal`. Clean them up by deleting old files/directories in
    `~/.cabal/logs/ghc-X.Y.Z`
    `~/.cabal/store/ghc-X.Y.Z`
    `~/.cabal/store/ghc-X.Y.Z/incoming`
    `~/.cabal/store/ghc-X.Y.Z/package.db`
Or delete all of `~/.cabal/store` and `~/.cabal/logs`, but that means all packages still in use will have to be downloaded and rebuilt.

# Alternatives

- https://hackage.haskell.org/package/shelly
- https://hackage.haskell.org/package/shell-conduit
- https://hackage.haskell.org/package/shh
- https://hackage.haskell.org/package/hsshellscript
- https://hackage.haskell.org/package/HSH
