{ pkgs ? import <nixpkgs> { } }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacsUnstable;
  alwaysEnsure = false;
  alwaysTangle = false;
}
