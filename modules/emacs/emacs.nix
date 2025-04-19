{ pkgs ? import <nixpkgs> { } }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = (pkgs.emacs30.override { withNativeCompilation = false; });
  alwaysEnsure = false;
  alwaysTangle = false;

  extraEmacsPackages = epkgs: with epkgs; [
    treesit-grammars.with-all-grammars
  ];
}
