{ pkgs, config }:

let server_packages = with pkgs;
  if "$HOME" == "nixpkgs.local" then
  [ nix-serve
  ]
  else
    [];
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

  autossh
  awscli
  cabal2nix
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
  openssl_1_1
  wget
] ++ server_packages
