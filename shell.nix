{ ps ? import <nixpkgs> {}
, nixpkgs ?  import (ps.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "f75d62941d8ad616bfc334ae19d71a0a6677dd19";
    sha256 = "0n88r4fw1syad9zl7r40r7xlxwx38ni8s9hzyayxssr21ii2p38h";
}) {}
, nixpkgsCabal3 ?  import (ps.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "5272327b81ed355bbed5659b8d303cf2979b6953";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
}) {}
, ghcVersion ? "ghc882"
# did'nt have time to look how to add benchmark but it should not be a big deal
# , doBenchmark ? false
}:
with nixpkgs;
let
  compiler = haskell.packages.${ghcVersion};
  drv = compiler.callPackage ./project.nix {};
  hie =
    ((import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {})
    .selection { selector = p: { ${ghcVersion} = p.${ghcVersion}; }; });

in
drv.env.overrideAttrs (shellEnv: {
  buildInputs = shellEnv.buildInputs ++ [
    nixpkgsCabal3.haskell.packages.${ghcVersion}.cabal-install
    cabal2nix
    hie
    (vscode-utils.vscodeEnv {
      #usually this path go to tmp but feel free to change it wherever you think it is good place to the "fake" global folder
      #putting it on empty string will leave the default one.
      user-data-dir = "./.vscode-globalUserData";
      #this file will be updated automaticly by vscode when using vscode UI to install/uninstall extensions
      mutableExtensionsFile = ./mutable-extensions.nix;
      nixExtensions = [
        { name = "vscode-hie-server"; publisher = "alanz"; version = "0.0.40"; sha256 = "1cmlgidjma41s5zq5161gcxxmk5lfzcm8dvznls04y5l7q9b0gca";  }
        { name = "language-haskell"; publisher = "justusadam"; version = "3.2.0"; sha256 = "190h1hky2yy5n00ncqf15mmaizgpm3w9pzvasmi2gangpg4qb6y5";  }
      ];

      # configuration for settings files- those will be overrides/create over the one in .vscode folder
      # settings = {};
      # keybindings ={};
    })
    git
    atom
  ];

  shellHook = ''
    echo 'to start editor with HIE run on the root of the folder: code .'
  '';
})
