{ pkgs ? import <nixpkgs> {} }:

let
  localconfig = import ./local.nix;
  server_packages = with pkgs;
  if localconfig.hostname == "Nixpkgs" then
  [
    adoptopenjdk-jre-openj9-bin-11
    jenkins
  ]
  else
    [];
in
with pkgs; [
  (import ./emacs.nix { inherit pkgs; })

  diffstat
  diffutils
  gitRepo
  gitAndTools.git-imerge
  (gitAndTools.gitFull.override { sendEmailSupport = true; })
  gitAndTools.gitflow
  gitstats
  nix-prefetch-git
  mercurialFull
  patch
  patchutils

  ag
  alacritty
  autossh
  cabal2nix
  curl
  ctags
  direnv
  editorconfig-core-c
  entr
  fswatch
  fzf
  gnugrep
  gnupg
  htop
  imgcat
  jq
  minify
  mosh
  openssl_1_1
  ripgrep
  wget
] ++ server_packages
