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
  exercism
  eza
  fd
  fzf
  git-credential-manager
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
  rustup
  sd
  silver-searcher
  tokei
  wakatime
  zotero
]
