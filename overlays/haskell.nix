self: pkgs:

{
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc865 = pkgs.haskell.packages.ghc865.override {
        overrides = self: super: with pkgs.haskell.lib; {
          HaskellNet        = doJailbreak super.HaskellNet;
          QuickCheck        = dontCheck super.QuickCheck;
          Glob              = dontCheck super.Glob;
          cron              = dontCheck super.cron;
          comonad           = dontCheck super.comonad;
          hakyll            = dontCheck (doJailbreak super.hakyll);
          http-types        = dontCheck super.http-types;
          lens              = dontCheck super.lens;
          pdf-toolbox-content = self.callCabal2nix "pdf-toolbox-content" (pkgs.fetchgit {
              url = "https://github.com/Yuras/pdf-toolbox.git";
              rev = "67ff8877c5445172072ef7b96ae6edc3ee06ee5e";
              sha256 = "15yj3dk4rywhwksizxbnvrnvin7my0kjp25050sjwdf3mn50mjmn";
            } + "/content") {};
          pdf-toolbox-core  = self.callCabal2nix "pdf-toolbox-core" (pkgs.fetchgit {
              url = "https://github.com/Yuras/pdf-toolbox.git";
              rev = "67ff8877c5445172072ef7b96ae6edc3ee06ee5e";
              sha256 = "15yj3dk4rywhwksizxbnvrnvin7my0kjp25050sjwdf3mn50mjmn";
            } + "/core") {};
          pdf-toolbox-document  = self.callCabal2nix "pdf-toolbox-document" (pkgs.fetchgit {
              url = "https://github.com/Yuras/pdf-toolbox.git";
              rev = "67ff8877c5445172072ef7b96ae6edc3ee06ee5e";
              sha256 = "15yj3dk4rywhwksizxbnvrnvin7my0kjp25050sjwdf3mn50mjmn";
            } + "/document") {};
          semigroupoids     = dontCheck super.semigroupoids;
          servant           = dontCheck super.servant;
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
