{ ghc ? "ghc902" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    fourmolu
    haskell-language-server
    hlint
    prim-bytearray; 
    
  inherit (pkgs) 
    cabal-install 
    clang 
    llvm;

}

