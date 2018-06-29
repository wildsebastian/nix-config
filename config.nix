{ pkgs }: {
  allowUnfree = true;

  packageOverrides = super: let
    self = super.pkgs;
    myHaskellPackages = import ./haskell.nix self.pkgs; in rec {

    otherHackagePackages =  libProf: self: super:
      with pkgs.haskell.lib; let pkg = self.callPackage; in rec {
      Agda                      = dontHaddock super.Agda;
      diagrams-contrib          = doJailbreak super.diagrams-contrib;
      diagrams-graphviz         = doJailbreak super.diagrams-graphviz;
      diagrams-svg              = doJailbreak super.diagrams-svg;
      # esqueleto               = dontCheck (doJailbreak super.esqueleto);
      hasktags                  = dontCheck super.hasktags;
      # hasql-cursor-transaction  = doJailbreak super.hasql-cursor-transaction;
      # hasql-migration           = dontCheck (doJailbreak super.hasql-migration);
      hspec-hedgehog            = dontCheck super.hspec-hedgehog;
      pipes-binary              = doJailbreak super.pipes-binary;
      pipes-group               = doJailbreak super.pipes-group;
      pipes-zlib                = dontCheck (doJailbreak super.pipes-zlib);
      servant                   = doJailbreak super.servant;
      servant-auth-server       = dontCheck (doJailbreak super.servant-auth-server);
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
      ];
    };

    coq88Env = with self.pkgs; self.pkgs.myEnvFun {
      name = "coq88";
      buildInputs = [
        coq_8_8
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
        myemacs
        vim
      ];
    };

    languageTools = with self.pkgs; buildEnv {
      name = "languageTools";
      paths = [
        cabal-install
        cabal2nix
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