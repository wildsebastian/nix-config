self: super:
{
  haskell = super.haskell // {
    packageOverrides = selfH: superH: rec {
      QuickCheck    = self.haskell.lib.dontCheck superH.QuickCheck;
      Glob          = self.haskell.lib.dontCheck superH.Glob;
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
