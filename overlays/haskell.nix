self: pkgs:

{
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc884 = pkgs.haskell.packages.ghc884.override {
        overrides = self: super: with pkgs.haskell.lib; {
          pandoc            = dontCheck (doJailbreak (self.callPackage ~/.nixpkgs/haskell-packages/pandoc-2.10.1.nix {}));
          hakyll            = dontCheck (doJailbreak (doDistribute super.hakyll_4_13_4_1));
          squeal-postgresql = self.callPackage ~/.nixpkgs/haskell-packages/squeal-0.6.0.2.nix {};
          math-functions    = dontCheck super.math-functions;
          rebase            = doJailbreak super.rebase;
        };
      };

      ghc8102 = pkgs.haskell.packages.ghc8102.override {
        overrides = self: super: with pkgs.haskell.lib; {
          haskeline         = dontCheck (self.callPackage ~/.nixpkgs/haskell-packages/haskeline-0.8.1.0.nix {});
          file-embed        = dontCheck (self.callPackage ~/.nixpkgs/haskell-packages/file-embed-0.0.11.2.nix {});
          http-media        = doJailbreak super.http-media;
          math-functions    = dontCheck super.math-functions;
          pandoc            = dontCheck (doJailbreak (self.callPackage ~/.nixpkgs/haskell-packages/pandoc-2.10.1.nix {}));
          rebase            = doJailbreak super.rebase;
          squeal-postgresql = dontCheck super.squeal-postgresql;
        };
      };

      ghcjs = pkgs.haskell.packages.ghcjs.override {
        overrides = self: super: with pkgs.haskell.lib; {
          haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
          rebase                = doJailbreak super.rebase;
        };
      };
    };
  };
}
