self: pkgs:

{
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc865 = pkgs.haskell.packages.ghc865.override {
        overrides = self: super: with pkgs.haskell.lib; {
          HaskellNet            = doJailbreak super.HaskellNet;
          QuickCheck            = dontCheck super.QuickCheck;
          Glob                  = dontCheck super.Glob;
          cron                  = dontCheck super.cron;
          comonad               = dontCheck super.comonad;
          http-types            = dontCheck super.http-types;
          lens                  = dontCheck super.lens;
          math-functions        = dontCheck super.math-functions;
          rebase                = doJailbreak super.rebase;
          semigroupoids         = dontCheck super.semigroupoids;
          servant               = dontCheck super.servant;
          servant-auth-server   = doJailbreak super.servant-auth-server;
          servant-server        = dontCheck (doJailbreak super.servant-server);
          time-compat           = dontCheck super.time-compat;
        };
      };

      ghc884 = pkgs.haskell.packages.ghc884.override {
        overrides = self: super: with pkgs.haskell.lib; {
          hakyll = self.callPackage ~/.nixpkgs/haskell-packages/hakyll-4.13.4.1.nix {};
          math-functions    = dontCheck super.math-functions;
          rebase            = doJailbreak super.rebase;
          squeal-postgresql = dontCheck super.squeal-postgresql;
        };
      };

      ghc8101 = pkgs.haskell.packages.ghc883.override {
        overrides = self: super: with pkgs.haskell.lib; {
          http-media        = doJailbreak super.http-media;
          math-functions    = dontCheck super.math-functions;
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
