{ mkDerivation, base, containers, generic-lens, lens, stdenv, text
}:
mkDerivation {
  pname = "Everything-Manager";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base containers generic-lens lens text ];
  executableHaskellDepends = [
    base containers generic-lens lens text
  ];
  doHaddock = false;
  description = "System used to optimize the efficiency of achieving goals";
  license = stdenv.lib.licenses.mit;
}
