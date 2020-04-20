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
          haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
          hakyll                = dontCheck (doJailbreak super.hakyll);
          http-types            = dontCheck super.http-types;
          lens                  = dontCheck super.lens;
          semigroupoids         = dontCheck super.semigroupoids;
          servant               = dontCheck super.servant;
          servant-auth-server   = doJailbreak super.servant-auth-server;
          servant-server        = dontCheck (doJailbreak super.servant-server);
          time-compat           = dontCheck super.time-compat;
        };
      };

      ghcjs = pkgs.haskell.packages.ghcjs.override {
        overrides = self: super: with pkgs.haskell.lib; {
          haddock-library-ghcjs = dontCheck super.haddock-library-ghcjs;
        };
      };
    };
  };
}
