{ pkgs ? import <nixpkgs> { } }:

with pkgs; [
  # agda
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
  minify
  mosh
  neofetch
  ngrok
  nix-direnv
  nushell
  openssl_1_1
  procs
  python39
  ripgrep
  sd
  silver-searcher
  tokei
  wireshark
  wget2
]
