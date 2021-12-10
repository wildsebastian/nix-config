self: pkgs:

{
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc884 = pkgs.haskell.packages.ghc884.override {
        overrides = self: super: with pkgs.haskell.lib; {
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
          junit-xml           = dontCheck super.junit-xml;
          math-functions      = dontCheck super.math-functions;
          rebase              = doJailbreak super.rebase;
          servant-client-core = dontCheck (doJailbreak super.servant-client-core);
          servant-client      = dontCheck (doJailbreak super.servant-client);
          squeal-postgresql   = dontCheck super.squeal-postgresql;
        };
      };

      ghc921 = pkgs.haskell.packages.ghc921.override {
        overrides = self: super: with pkgs.haskell.lib; {
          HsYAML-aeson        = doJailbreak super.HsYAML-aeson;
          cabal-doctest       = doJailbreak super.cabal-doctest;
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
