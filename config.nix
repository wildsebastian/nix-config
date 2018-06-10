{ pkgs }: {
  allowUnfree = true;

  packageOverrides = super: let
    self = super.pkgs;
    myHaskellPackages = import ./haskell.nix self.pkgs; in rec {

    otherHackagePackages =  libProf: self: super:
      with pkgs.haskell.lib; let pkg = self.callPackage; in rec {
      Agda                = dontHaddock super.Agda;
      diagrams-contrib    = doJailbreak super.diagrams-contrib;
      diagrams-graphviz   = doJailbreak super.diagrams-graphviz;
      diagrams-svg        = doJailbreak super.diagrams-svg;
      hasktags            = dontCheck super.hasktags;
      hspec-hedgehog      = dontCheck super.hspec-hedgehog;
      pipes-binary        = doJailbreak super.pipes-binary;
      pipes-zlib          = dontCheck (doJailbreak super.pipes-zlib);
      servant-auth-server = dontCheck (doJailbreak super.servant-auth-server);
      text-show           = dontCheck (doJailbreak super.text-show);
      time-recurrence     = doJailbreak super.time-recurrence;
    };

    haskell822Packages = self.haskell.packages.ghc822.override {
      overrides = otherHackagePackages false;
    };

    haskell842Packages = self.haskell.packages.ghc842.override {
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
      buildInputs = with haskell842Packages; [
        (ghcWithHoogle (myHaskellPackages 8.4))
        alex happy
        ghc-core
        ghcid
        hlint
        hasktags
      ];
    };

    jsEnv = with self.pkgs; self.pkgs.myEnvFun {
      name = "js";
      buildInputs = [
        nodejs
        yarn
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
        emacs25Macport
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

    systemPackages = with self.pkgs; buildEnv {
      name = "systemPackages";
      paths = [
        curl
        gnupg
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
