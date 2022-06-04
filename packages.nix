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
  pi4_packages = with pkgs;
  if localconfig.system == "pi" then
  [
    libraspberrypi
  ]
  else
    [];
in
with pkgs; [
  agda
  (import ./emacs.nix { inherit pkgs; })

  diffstat
  diffutils
  gitRepo
  gitAndTools.delta
  gitAndTools.git-imerge
  (gitAndTools.gitFull.override { sendEmailSupport = true; })
  gitAndTools.gitflow
  gitstats
  nix-prefetch-git
  mercurialFull
  patch
  patchutils

  alacritty
  autossh
  (aspellWithDicts (d: [d.de d.en d.it]))
  bat
  bottom
  cabal2nix
  curl
  ctags
  direnv
  du-dust
  editorconfig-core-c
  exa
  fd
  fzf
  gnugrep
  gnupg
  htop
  imgcat
  jq
  macchina
  minify
  mosh
  neofetch
  # neovim
  ngrok
  nushell
  openssl_1_1
  # procs
  python39
  ripgrep
  sd
  silver-searcher
  starship
  tokei
  wget2
] ++ server_packages ++ pi4_packages
