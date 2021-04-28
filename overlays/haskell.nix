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

      ghc8104 = pkgs.haskell.packages.ghc8104.override {
        overrides = self: super: with pkgs.haskell.lib; {
          chronos             = dontCheck super.chronos;
          hakyll              = dontCheck (doJailbreak super.hakyll);
          http-media          = doJailbreak super.http-media;
          math-functions      = dontCheck super.math-functions;
          pandoc              = dontCheck (doJailbreak (self.callPackage ~/.nixpkgs/haskell-packages/pandoc-2.11.4.nix {}));
          rebase              = doJailbreak super.rebase;
          servant-client-core = dontCheck (doJailbreak super.servant-client-core);
          servant-client      = dontCheck (doJailbreak super.servant-client);
          squeal-postgresql   = dontCheck super.squeal-postgresql;
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
