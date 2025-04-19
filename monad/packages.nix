{ pkgs ? import <nixpkgs> { } }:

with pkgs; [
  pam-reattach

  (import ../modules/emacs/emacs.nix { inherit pkgs; })

  nix-prefetch-git

  azure-cli
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
  kubectl
  nixos-rebuild
  openssl
  ripgrep
  tokei
]
