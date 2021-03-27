#! /usr/bin/env nix-shell
#! nix-shell -i runghc --packages "ghc.withPackages (x: [ x.turtle ])"

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = echo "Hello, world!"

-- install nix
--   check if nix is installed
--   if not run:
--   curl -L https://nixos.org/nix/install | sh
--   if haskell not installed, add haskell 8.10.4 to config nix
-- setup database
