{ pkgs, config }:

with pkgs; [
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

  weechat

  autossh
  curl
  ctags
  direnv
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
  wget
]
