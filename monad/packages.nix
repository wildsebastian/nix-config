{ pkgs ? import <nixpkgs> { } }:

let
  zotero = pkgs.callPackage ../darwin/zotero.nix { };
in
with pkgs; [
  agda
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
  exa
  fd
  firefox-bin
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
  openssl_1_1
  procs
  python39
  ripgrep
  rnix-lsp
  sd
  silver-searcher
  tokei
  wget2
  zotero
]
