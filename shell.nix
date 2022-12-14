{ ghc ? "ghc922" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.prim-bytearray.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [ 
    pkgs.cabal-install
    pkgs.clang
    pkgs.fourmolu
    pkgs.haskell-language-server
    pkgs.llvm
  ];
})
