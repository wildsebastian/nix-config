{ pkgs ? import <nixpkgs> { } }:

let
  zotero = pkgs.callPackage ../darwin/zotero.nix { };
in
with pkgs; [
  pam-reattach

  (agda.withPackages (p: [
    p.standard-library
  ]))
  (import ../modules/emacs/emacs.nix { inherit pkgs; })

  diffstat
  diffutils
  gitRepo
  gitAndTools.git-imerge
  gitAndTools.gitflow
  gitstats
  nix-prefetch-git
  mercurialFull
  patch
  patchutils

  asitop
  autossh
  (aspellWithDicts (d: [ d.de d.en d.it ]))
  bat
  bottom
  cabal2nix
  cachix
  curl
  ctags
  delta
  du-dust
  editorconfig-core-c
  php74
  eza
  fd
  fzf
  gnugrep
  gnupg
  htop
  imgcat
  jq
  minify
  mosh
  neofetch
  ngrok
  nixos-rebuild
  nushell
  openssl
  procs
  python39
  ripgrep
  rnix-lsp
  sd
  silver-searcher
  tokei
  vagrant
  wakatime
  zotero
]
