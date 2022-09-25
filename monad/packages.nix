{ pkgs ? import <nixpkgs> {} }:

with pkgs; [
  # agda
  (import ../modules/emacs/emacs.nix { inherit pkgs; })

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
  minify
  mosh
  neofetch
  neovim
  ngrok
  nix-direnv
  nushell
  openssl_1_1
  procs
  python39
  ripgrep
  sd
  silver-searcher
  starship
  tokei
  wget2
]
