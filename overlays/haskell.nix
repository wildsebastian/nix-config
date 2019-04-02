self: super:
{
  haskell = super.haskell // {
    packageOverrides = selfH: superH: rec {
      cron = self.haskell.lib.dontCheck superH.cron;
    };
  };
}
