{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs25Macport;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    zerodark-theme
  ]) ++ (with epkgs.melpaPackages; [
    ac-haskell-process
    auto-complete
    base16-theme
    color-theme-solarized
    dashboard
    evil
    evil-magit
    flycheck
    haskell-mode
    magit
    multiple-cursors
    neotree
    nix-mode
    page-break-lines
    projectile
    rainbow-delimiters
    restclient
    use-package
    whitespace-cleanup-mode
  ]) ++ (with epkgs.elpaPackages; [
    auctex
    beacon
    company
    nameless
    undo-tree
  ]))
