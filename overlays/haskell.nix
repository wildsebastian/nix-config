self: pkgs:

{
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc8107 = pkgs.haskell.packages.ghc8107.override {
        overrides = self: super: with pkgs.haskell.lib; {
          squeal-postgresql   = dontCheck super.squeal-postgresql;
        };
      };
    };
  };
}
