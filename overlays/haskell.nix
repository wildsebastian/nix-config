self: pkgs:

{
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc92 = pkgs.haskell.packages.ghc92.override {
        overrides = self: super: with pkgs.haskell.lib; {
          fsnotify = dontCheck super.fsnotify;
        };
      };
    };
  };
}
