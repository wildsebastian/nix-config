{ pkgs ? import <nixpkgs> { } }:

with pkgs; [
  bat
  bottom
  curl
  direnv
  du-dust
  exa
  fd
  fzf
  git
  htop
  jq
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
