#!/usr/bin/bash

if ! command -v ghc &> /dev/null
then
    if ! command -v nix-env &> /dev/null
    then
        curl -L https://nixos.org/nix/install | sh
    fi
    nix-env -i unstable.ghcid haskell.compiler.ghc8104 haskellPackages.cabal-install
fi
