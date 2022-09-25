{ pkgs ? import <nixpkgs> { } }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacsNativeComp;
  alwaysEnsure = false;
  alwaysTangle = false;
}
