{ pkgs ? import <nixpkgs> {} }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacsGcc;
  alwaysEnsure = false;
  alwaysTangle = false;
}
