{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    magit
    zerodark-theme
  ]) ++ (with epkgs.melpaPackages; [
    ac-haskell-process
    auto-complete
    base16-theme
    color-theme-solarized
    dashboard
    evil
    flycheck
    haskell-mode
    multiple-cursors
    neotree
    nix-mode
    page-break-lines
    projectile
    rainbow-delimiters
    use-package
    whitespace-cleanup-mode
  ]) ++ (with epkgs.elpaPackages; [
    auctex
    beacon
    company
    nameless
    undo-tree
  ]))
