#!/usr/bin/bash

command_not_exists () {
    ! command -v $1 &> /dev/null
}

install_nix () {
    if command_not_exists nix-env
    then
        curl -L https://nixos.org/nix/install | sh
        . /home/ben/.nix-profile/etc/profile.d/nix.sh
    fi
    if [ ! -f ~/.config/nixpkgs/config.nix ];
    then
        mkdir -p ~/.config/nixpkgs/
        echo -n "with import <nixpkgs> {};

        let
            unstable = import <nixpkgs> { inherit config; };
        in
          {
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

if command_not_exists curl
then
    echo "The program curl is required to run this script."
    exit
fi
if command_not_exists ghc
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
if command_not_exists pg_config
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

if command_not_exists cachix
then
    nix-env -iA cachix -f https://cachix.org/api/v1/install
fi
cachix use everythingmanager
nix-shell
cabal update
cabal install --only-dependencies
cabal build
echo "Setup complete. Run the project with cabal repl"
