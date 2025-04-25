# Installation

Create [local repository][https://cabal.readthedocs.io/en/stable/config.html#local-no-index-repositories]:
    `mkdir "$HOME/.cabal/lepository"`
    Add following lines to cabal config file (usually `~/.cabal/config`)
        `repository lepository`
        `    url: file+noindex:///home/your_username/.cabal/lepository`
    To use library in a project, specify it as a dependency in `yourproject.cabal`:
        `build-depends: claw`
Install library and executable
    `install.sh`
Build documentation (optional)
    `cabal haddock`

# Cleanup

Old packages accumulate in `~/.cabal`. Clean them up by deleting old files/directories in
    `~/.cabal/logs/ghc-X.Y.Z`
    `~/.cabal/store/ghc-X.Y.Z`
    `~/.cabal/store/ghc-X.Y.Z/incoming`
    `~/.cabal/store/ghc-X.Y.Z/package.db`
Or delete all of `~/.cabal/store` and `~/.cabal/logs`, but that means all packages still in use will have to be downloaded and built again.

# Alternatives

- https://hackage.haskell.org/package/shelly
- https://hackage.haskell.org/package/shell-conduit
- https://hackage.haskell.org/package/shh
- https://hackage.haskell.org/package/hsshellscript
- https://hackage.haskell.org/package/HSH
