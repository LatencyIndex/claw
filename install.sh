#!/usr/bin/env bash


## Install claw library
# Create local repo directory if it doesn't already exist
REPO="$HOME/.cabal/lepository"
mkdir -p -- "$REPO"
# Place library .tar.gz source package into local noindex repository
cabal sdist --output-directory "$REPO"
# Clear now-stale repository cache
rm -- "$REPO/noindex.cache"
# Update/refresh cabal (unsure if required)
# cabal update

## Install claw executable
# Create claw's config directory if it doesn't already exist
CONFIG="$HOME/.config/claw"
mkdir -p -- "$CONFIG"
# Build & place claw executable on system path
cabal install --overwrite-policy=always
# Copy init script to claw's config directory, where the executable can find it
cp -- "data/init.ghci" "$CONFIG"
