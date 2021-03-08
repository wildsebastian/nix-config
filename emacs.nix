{ pkgs ? import <nixpkgs> {} }:

emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacsGcc;
  alwaysEnsure = false;
  alwaysTangle = false;
}
