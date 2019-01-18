{ mkDerivation, base, capability, stdenv, transformers }:
mkDerivation {
  pname = "endo-machine";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base capability transformers ];
  description = "Synopsis";
  license = stdenv.lib.licenses.bsd3;
}
