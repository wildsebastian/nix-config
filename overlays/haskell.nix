self: pkgs:

{
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc865 = pkgs.haskell.packages.ghc865.override {
        overrides = self: super: with pkgs.haskell.lib; {
          HaskellNet          = doJailbreak super.HaskellNet;
          QuickCheck          = dontCheck super.QuickCheck;
          Glob                = dontCheck super.Glob;
          cron                = dontCheck super.cron;
          comonad             = dontCheck super.comonad;
          hakyll              = dontCheck (doJailbreak super.hakyll);
          http-types          = dontCheck super.http-types;
          lens                = dontCheck super.lens;
          semigroupoids       = dontCheck super.semigroupoids;
          servant             = dontCheck super.servant;
          servant-auth-server = doJailbreak super.servant-auth-server;
          time-compat         = dontCheck super.time-compat;
        };
      };

      ghc881 = pkgs.haskell.packages.ghc881.override {
        overrides = self: super: with pkgs.haskell.lib; {
          HaskellNet = self.callCabal2nix "HaskellNet" (pkgs.fetchgit {
            url = "https://github.com/jtdaugherty/HaskellNet.git";
            rev = "af38595ec1bd804bdc365d103c9142f595e6295c";
            sha256 = "15zmbhhh2d8hxcgmz3z1xb19rhbhhfwn8nrpqxqrx9k37zsn6z77";
          }) {};
          http-media = doJailbreak super.http-media;
        };
      };
    };
  };
}
