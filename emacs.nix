{ pkgs ? import <nixpkgs> {} }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacsGit;
  alwaysEnsure = false;
  alwaysTangle = false;
}
