{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-bool";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bool";
    sha256 = "08qpjpyad9nk8bdsj71207msa7r9wciyggy3pxw0jbj2l27bb55w";
    rev = "095cc4a211e7da95a10cccaccb0290eb29d60dc3";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base ghc-prim prim-compat template-haskell
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-bool";
  description = "Unboxed booleans";
  license = lib.licenses.isc;
}
