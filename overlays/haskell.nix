self: super:
{
  haskell = super.haskell // {
    packageOverrides = self: super: rec {
      cron = self.haskell.lib.dontCheck super.cron;
    };
  };
}
