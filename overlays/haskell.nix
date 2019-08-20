self: super:
{
  haskell = super.haskell // {
    packageOverrides = selfH: superH: rec {
      QuickCheck    = self.haskell.lib.dontCheck superH.QuickCheck;
      Glob          = self.haskell.lib.dontCheck superH.Glob;
#      cachix        = self.haskell.lib.overrideCabal superH.cachix (drv: {
#        src = super.fetchgit {
#          url = "https://github.com/cachix/cachix";
#          sha256 = "1kcrl1k1ac11w956mdarnjavrdai664j76qcdjhd234sabilva0z";
#          rev = "ea09098a84c4c3909f0d76f1a0e8eafa2db6ef28";
#          fetchSubmodules = true;
#        };
#      });
      cron          = self.haskell.lib.dontCheck superH.cron;
      comonad       = self.haskell.lib.dontCheck superH.comonad;
      hakyll        = self.haskell.lib.dontCheck (self.haskell.lib.doJailbreak superH.hakyll);
      http-types    = self.haskell.lib.dontCheck superH.http-types;
      lens          = self.haskell.lib.dontCheck superH.lens;
      semigroupoids = self.haskell.lib.dontCheck superH.semigroupoids;
      servant       = self.haskell.lib.dontCheck superH.servant;
    };
  };
}
