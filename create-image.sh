#!/bin/bash

# builds haskell image with ghc 9.2.7.0 and cabal 3.6.2.0 locally and tags it
# this gets cached, so can just keep it in the script.
docker build -t haskell:ghc-9.2.7.0-cabal-3.6.2.0 -f ./haskell-dockerfile .
