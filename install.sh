#!/usr/bin/bash

install_nix () {
    if ! command -v nix-env &> /dev/null
    then
        curl -L https://nixos.org/nix/install | sh
        . /home/ben/.nix-profile/etc/profile.d/nix.sh
    fi
    if [ ! -f ~/.config/nixpkgs/config.nix ];
    then
        mkdir -p ~/.config/nixpkgs/
        echo -n "with import <nixpkgs> {};

        let
            all-hies = import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\") {};
            unstable = import <nixpkgs> { inherit config; };
        in
          {
            allowUnfree = true;
            allowBroken = true;
        
            packageOverrides = pkgs: rec {
              all = pkgs.buildEnv {
                name = \"all\";
                paths = [
                  haskell.compiler.ghc8104
                  haskellPackages.cabal-install
                  unstable.lorri
                  unstable.direnv
                ];
              };
            };
          }
        " > ~/.config/nixpkgs/config.nix
        nix-env -i all
    else
    # not sure what to add here to install haskell with nix-env -i
        nix-env -i haskell.packages.ghc8104 unstable.lorri unstable.direnv
    fi
}

install_postgres () {
nix-env -i postgresql
createdb -U postgres testdb
psql -U postgres -d postgres
}

if ! command -v curl &> /dev/null
then
    echo "The program curl is required to run this script."
    exit
fi
if ! command -v ghc &> /dev/null
then
    while true; do
        read -p "It is detected that ghc isn't installed. Would you like to install it with nix? y/n:" yn
        case $yn in
            [Yy]* ) install_nix; break;;
            [Nn]* ) break;;
            * ) echo "Please answer yes or no.";;
        esac
    done
fi
if ! command -v pg_config &> /dev/null
then
    while true; do
        read -p "It is detected that you do not have postgres installed. Would you like to install it now with nix? y/n:" yn	
        case $yn in
            [Yy]* ) install_postgres; break;;
            [Nn]* ) break;;
            * ) echo "Please answer yes or no.";;
        esac
    done
fi

if ! command -v cachix &> /dev/null
then
    nix-env -iA cachix -f https://cachix.org/api/v1/install
fi
cachix use everythingmanager
nix-shell
cabal update
cabal install --only-dependencies
cabal build
echo "Setup complete. Run the project with cabal repl"
