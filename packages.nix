{ pkgs, config }:

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

  cabal-install
  (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak haskellPackages.cabal2nix))
  (haskellPackages.ghcid)
  (haskellPackages.hlint)
  (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak haskellPackages.patat))
  (haskellPackages.stylish-cabal)
  (haskellPackages.stylish-haskell)
  carnix

  closurecompiler
  coq_8_9
  elmPackages.elm
  elmPackages.elm-format
  nodejs
  ocaml
  purescript
  python2
  python3
  python37Packages.pip
  python37Packages.setuptools
  python37Packages.virtualenv
  watchman
  yarn

  postgresql_11

  weechat

  autossh
  curl
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
