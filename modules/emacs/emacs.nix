{ pkgs ? import <nixpkgs> { } }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacs-unstable;
  alwaysEnsure = false;
  alwaysTangle = false;

  extraEmacsPackages = epkgs: [
    epkgs.agda2-mode
    epkgs.idris2-mode
  ];
}
