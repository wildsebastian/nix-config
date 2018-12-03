{ pkgs }: {
  allowUnfree = true;

  packageOverrides = super: let
    self = super.pkgs;
    myHaskellPackages = import ./haskell.nix self.pkgs; in rec {

    otherHackagePackages =  libProf: self: super:
      with pkgs.haskell.lib; let pkg = self.callPackage; in rec {
      Agda                      = dontHaddock super.Agda;
      ListLike                  = dontCheck (doJailbreak super.ListLike);
      cabal-helper              = doJailbreak super.cabal-helper;
      diagrams-contrib          = doJailbreak super.diagrams-contrib;
      diagrams-graphviz         = doJailbreak super.diagrams-graphviz;
      diagrams-svg              = doJailbreak super.diagrams-svg;
      enclosed-exceptions       = dontCheck super.enclosed-exceptions;
      ghc-mod                   = dontCheck (doJailbreak super.ghc-mod);
      hasktags                  = dontCheck super.hasktags;
      hspec-hedgehog            = dontCheck super.hspec-hedgehog;
      jose                      = dontCheck (doJailbreak super.jose);
      lifted-base               = dontCheck (doJailbreak super.lifted-base);
      liquidhaskell             = doJailbreak super.liquidhaskell;
      pipes-binary              = doJailbreak super.pipes-binary;
      pipes-group               = doJailbreak super.pipes-group;
      pipes-zlib                = dontCheck (doJailbreak super.pipes-zlib);
      process-extras            = dontCheck (doJailbreak super.process-extras);
      semigroups                = dontCheck (doJailbreak super.semigroups);
      servant                   = doJailbreak super.servant;
      servant-auth-server       = dontCheck (doJailbreak super.servant-auth-server);
      servant-client            = dontCheck super.servant-client;
      servant-client-core       = dontCheck (doJailbreak super.servant-client-core);
      servant-server            = dontCheck (doJailbreak super.servant-server);
      tasty-hspec               = dontCheck (doJailbreak super.tasty-hspec);
      text-builder              = dontCheck (doJailbreak super.text-builder);
      text-show                 = dontCheck (doJailbreak super.text-show);
      time-recurrence           = doJailbreak super.time-recurrence;
      wl-pprint-annotated       = dontCheck (doJailbreak super.wl-pprint-annotated);
    };

    haskell822Packages = self.haskell.packages.ghc822.override {
      overrides = otherHackagePackages false;
    };

    haskell822PackagesProf = self.haskell.packages.ghc822.override {
      overrides = otherHackagePackages true;
    };

    ghc82EnvProf = self.pkgs.myEnvFun {
      name = "ghc82-prof";
      buildInputs = with haskell822PackagesProf; [
        (ghcWithHoogle (myHaskellPackages 8.2))
        alex happy cabal-install
        cabal-helper
        ghc-core
        ghcid
        hlint
        hasktags
        stylish-haskell
      ];
    };

     ghc84Env = self.pkgs.myEnvFun {
      name = "ghc82";
      buildInputs = with haskell822Packages; [
        (ghcWithHoogle (myHaskellPackages 8.2))
        alex happy cabal-install
        cabal-helper
        ghc-core
        ghcid
        hlint
        hasktags
        stack
        stylish-haskell
      ];
    };

    haskell844Packages = self.haskell.packages.ghc844.override {
      overrides = otherHackagePackages false;
    };

    haskell844PackagesProf = self.haskell.packages.ghc844.override {
      overrides = otherHackagePackages true;
    };

    ghc84EnvProf = self.pkgs.myEnvFun {
      name = "ghc84-prof";
      buildInputs = with haskell844PackagesProf; [
        (ghcWithHoogle (myHaskellPackages 8.4))
        alex happy cabal-install
        cabal-helper
        ghc-core
        ghcid
        hlint
        hasktags
        stylish-haskell
      ];
    };

     ghc84Env = self.pkgs.myEnvFun {
      name = "ghc84";
      buildInputs = with haskell844Packages; [
        (ghcWithHoogle (myHaskellPackages 8.4))
        alex happy cabal-install
        cabal-helper
        ghc-core
        ghcid
        hlint
        hasktags
        stack
        stylish-haskell
      ];
    };

    haskell862Packages = self.haskell.packages.ghc862.override {
      overrides = otherHackagePackages false;
    };

    haskell862PackagesProf = self.haskell.packages.ghc862.override {
      overrides = otherHackagePackages true;
    };

    ghc86EnvProf = self.pkgs.myEnvFun {
      name = "ghc86-prof";
      buildInputs = with haskell862PackagesProf; [
        (ghcWithHoogle (myHaskellPackages 8.6))
        alex happy cabal-install
        ghc-core
        ghcid
        hlint
        hasktags
        (self.pkgs.haskell.lib.doJailbreak stylish-haskell)
      ];
    };

     ghc86Env = self.pkgs.myEnvFun {
      name = "ghc86";
      buildInputs = with haskell862Packages; [
        (ghcWithHoogle (myHaskellPackages 8.6))
        alex happy
        ghc-core
        ghcid
        hlint
        hasktags
        (self.pkgs.haskell.lib.doJailbreak stylish-haskell)
        stack
      ];
    };

    coq88Env = with self.pkgs; self.pkgs.myEnvFun {
      name = "coq88";
      buildInputs = [
        coq_8_8
      ];
    };

    ocamlEnv = with self.pkgs; self.pkgs.myEnvFun {
      name = "ocaml";
      buildInputs = [
        ocaml
      ];
    };

    elmEnv = with self.pkgs; self.pkgs.myEnvFun {
      name = "elm";
      buildInputs = with elmPackages; [
        elm
        elm-format
      ];
    };

    jsEnv = with self.pkgs; self.pkgs.myEnvFun {
      name = "js";
      buildInputs = [
        nodejs
        watchman
        yarn
      ];
    };

    dbEnv = with self.pkgs; self.pkgs.myEnvFun {
      name = "db";
      buildInputs = [
        postgresql_11
      ];
    };

    gitTools = with self.pkgs; buildEnv {
      name = "gitTools";
      paths = [
        diffstat
        diffutils
        gitRepo
        gitAndTools.git-imerge
        gitAndTools.gitFull
        gitAndTools.gitflow
        gitstats
        patch
        patchutils
      ];
    };

    editors = with self.pkgs; buildEnv {
      name = "editors";
      paths = [
        emacs-all-the-icons-fonts
        emacs
        vim
      ];
    };

    languageTools = with self.pkgs; buildEnv {
      name = "languageTools";
      paths = [
        cabal-install
        (self.pkgs.haskell.lib.dontCheck (self.pkgs.haskell.lib.doJailbreak cabal2nix))
        carnix
      ];
    };

    chatTools = with self.pkgs; buildEnv {
      name = "chatTools";
      paths = [
        weechat
      ];
    };

    systemPackages = with self.pkgs; buildEnv {
      name = "systemPackages";
      paths = [
        autossh
        curl
        fswatch
        gnupg
        imgcat
        libiconv
        mosh
        openssl_1_1
        python36Packages.powerline
        tmux
        wget
        zsh
      ];
    };
  };
}
