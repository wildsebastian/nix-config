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
      text-builder              = dontCheck (doJailbreak super.text-builder);
      text-show                 = dontCheck (doJailbreak super.text-show);
      time-recurrence           = doJailbreak super.time-recurrence;
    };

    haskell822Packages = self.haskell.packages.ghc822.override {
      overrides = otherHackagePackages false;
    };

    haskell843Packages = self.haskell.packages.ghc843.override {
      overrides = otherHackagePackages false;
    };

    ghc82Env = self.pkgs.myEnvFun {
      name = "ghc82";
      buildInputs = with haskell822Packages; [
        (ghcWithHoogle (myHaskellPackages 8.2))
        Agda
        alex happy cabal-install
        ghc-core
        ghcid
        hlint
        hasktags
        stylish-haskell
      ];
    };

    ghc84Env = self.pkgs.myEnvFun {
      name = "ghc84";
      buildInputs = with haskell843Packages; [
        (ghcWithHoogle (myHaskellPackages 8.4))
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

    jsEnv = with self.pkgs; self.pkgs.myEnvFun {
      name = "js";
      buildInputs = [
        nodejs
        yarn
      ];
    };

    dbEnv = with self.pkgs; self.pkgs.myEnvFun {
      name = "db";
      buildInputs = [
        postgresql100
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

    myemacs = import ./emacs.nix { pkgs = self; };

    editors = with self.pkgs; buildEnv {
      name = "editors";
      paths = [
        emacs-all-the-icons-fonts
        myemacs
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
        gnupg
        libiconv
        mosh
        openssl_1_1_0
        python36Packages.powerline
        tmux
        wget
        zsh
      ];
    };
  };
}
