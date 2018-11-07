{ pkgs }: {
  allowUnfree = true;

  packageOverrides = super: let
    self = super.pkgs;
    myHaskellPackages = import ./haskell.nix self.pkgs; in rec {

    otherHackagePackages =  libProf: self: super:
      with pkgs.haskell.lib; let pkg = self.callPackage; in rec {
      Agda                      = dontHaddock super.Agda;
      semigroups                = dontCheck (doJailbreak super.semigroups);
      ListLike                  = dontCheck (doJailbreak super.ListLike);
      cabal-helper              = doJailbreak super.cabal-helper;
      ghc-mod                   = dontCheck (doJailbreak super.ghc-mod);
      diagrams-contrib          = doJailbreak super.diagrams-contrib;
      diagrams-graphviz         = doJailbreak super.diagrams-graphviz;
      diagrams-svg              = doJailbreak super.diagrams-svg;
      enclosed-exceptions       = dontCheck super.enclosed-exceptions;
      hasktags                  = dontCheck super.hasktags;
      hspec-hedgehog            = dontCheck super.hspec-hedgehog;
      liquidhaskell             = doJailbreak super.liquidhaskell;
      pipes-binary              = doJailbreak super.pipes-binary;
      pipes-group               = doJailbreak super.pipes-group;
      pipes-zlib                = dontCheck (doJailbreak super.pipes-zlib);
      process-extras            = dontCheck (doJailbreak super.process-extras);
      servant                   = doJailbreak super.servant;
      servant-auth-server       = dontCheck (doJailbreak super.servant-auth-server);
      servant-client            = dontCheck super.servant-client;
      text-builder              = dontCheck (doJailbreak super.text-builder);
      text-show                 = dontCheck (doJailbreak super.text-show);
      time-recurrence           = doJailbreak super.time-recurrence;
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
        stylish-haskell
      ];
    };

    haskell861Packages = self.haskell.packages.ghc861.override {
      overrides = otherHackagePackages false;
    };

    haskell861PackagesProf = self.haskell.packages.ghc861.override {
      overrides = otherHackagePackages true;
    };

    ghc86EnvProf = self.pkgs.myEnvFun {
      name = "ghc86-prof";
      buildInputs = with haskell861PackagesProf; [
        (ghcWithHoogle (myHaskellPackages 8.6))
        alex happy cabal-install
        ghc-core
        ghcid
        hlint
        hasktags
        stylish-haskell
      ];
    };

     ghc86Env = self.pkgs.myEnvFun {
      name = "ghc86";
      buildInputs = with haskell861Packages; [
        (ghcWithHoogle (myHaskellPackages 8.6))
        alex happy
        ghc-core
        ghcid
        hlint
        hasktags
        stylish-haskell
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
