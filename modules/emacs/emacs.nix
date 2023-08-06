{ pkgs ? import <nixpkgs> { } }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacs-unstable;
  alwaysEnsure = false;
  alwaysTangle = false;

  extraEmacsPackages = epkgs: with epkgs; [
    agda2-mode
    idris2-mode
    treesit-grammars.with-all-grammars
  ];
}
