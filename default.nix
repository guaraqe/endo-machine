{ mkDerivation, base, capability, lens, stdenv, transformers }:
mkDerivation {
  pname = "endo-machine";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base capability lens transformers ];
  testHaskellDepends = [ base capability lens ];
  description = "Synopsis";
  license = stdenv.lib.licenses.bsd3;
}
