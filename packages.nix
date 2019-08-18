{ pkgs, config }:

let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
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
  mercurialFull
  patch
  patchutils

  (all-hies.selection { selector = p: { inherit (p) ghc844 ghc865; }; })
  cachix
  python3

  autossh
  cachix
  curl
  ctags
  direnv
  editorconfig-core-c
  entr
  fswatch
  fzf
  gnupg
  htop
  imgcat
  jq
  minify
  mosh
  nixops
  openssl_1_1
  weechat
  wget
]
