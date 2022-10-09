{ pkgs ? import <nixpkgs> { } }:

with pkgs; [
  bat
  bottom
  curl
  direnv
  docker-compose
  du-dust
  exa
  fd
  fzf
  git
  htop
  jq
  macchina
  minify
  mosh
  neovim
  nushell
  openssl_1_1
  procs
  ripgrep
  sd
  starship
  tokei
  wget2
]
