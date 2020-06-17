{ mkDerivation, base, stdenv, text, containers }:
mkDerivation {
  pname = "Everything-Manager";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base text containers ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
