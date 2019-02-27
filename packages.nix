{ pkgs, config }:

with pkgs; [
    (import ./emacs.nix { inherit pkgs; })

    diffstat
    diffutils
    gitRepo
    gitAndTools.git-imerge
    gitAndTools.gitFull
    gitAndTools.gitflow
    gitstats
    mercurialFull
    patch
    patchutils

    cabal-install
    (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak cabal2nix))
    (haskellPackages_8_6.ghcid)
    (haskellPackages_8_6.hlint)
    carnix

    coq_8_9
    elmPackages.elm
    elmPackages.elm-format
    nodejs
    ocaml
    purescript
    python2
    python3
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
