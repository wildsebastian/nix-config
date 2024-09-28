{ pkgs ? import <nixpkgs> { } }:

with pkgs; [
  bat
  bottom
  curl
  delta
  direnv
  du-dust
  eza
  fd
  fzf
  git
  htop
  jq
  minify
  mosh
  neovim
  nushell
  openssl
  procs
  ripgrep
  sd
  starship
  tokei
]
