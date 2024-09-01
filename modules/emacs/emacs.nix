{ pkgs ? import <nixpkgs> { } }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacs29;
  alwaysEnsure = false;
  alwaysTangle = false;

  extraEmacsPackages = epkgs: with epkgs; [
    idris2-mode
    treesit-grammars.with-all-grammars
  ];
}
