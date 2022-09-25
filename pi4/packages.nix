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
  k3s
  macchina
  minify
  mosh
  nushell
  openssl_1_1
  procs
  ripgrep
  sd
  starship
  tokei
  vim
  wget2
]
