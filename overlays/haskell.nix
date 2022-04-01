self: pkgs:

{
  haskell = pkgs.haskell // {
    packages = pkgs.haskell.packages // {
      ghc8107 = pkgs.haskell.packages.ghc8107.override {
        overrides = self: super: with pkgs.haskell.lib; {
          records-sop = doJailbreak super.records-sop;
          squeal-postgresql = dontCheck super.squeal-postgresql;
        };
      };
      ghc902 = pkgs.haskell.packages.ghc902.override {
        overrides = self: super: with pkgs.haskell.lib; {
          base64 = dontCheck super.base64;
        };
      };
    };
  };
}
