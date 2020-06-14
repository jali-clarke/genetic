{ mkDerivation, base, MonadRandom, parallel, stdenv, vector
, vector-algorithms
}:
mkDerivation {
  pname = "genetic";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base MonadRandom parallel vector vector-algorithms
  ];
  license = stdenv.lib.licenses.gpl3;
}
