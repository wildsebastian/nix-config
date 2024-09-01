{ pkgs ? import <nixpkgs> { } }:

with pkgs; [
  pam-reattach

  (import ../modules/emacs/emacs.nix { inherit pkgs; })

  nix-prefetch-git

  bat
  bottom
  delta
  du-dust
  editorconfig-core-c
  eza
  fd
  fzf
  gnupg
  jq
  nixos-rebuild
  openssl
  ripgrep
  tokei
  wakatime
]
