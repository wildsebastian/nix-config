{ pkgs }: {
  allowUnfree = true;

  packageOverrides = super: let
    self = super.pkgs;
    myHaskellPackages = import ./haskell.nix self.pkgs; in rec {
    myGhcJsPackages = import ./ghcjs.nix self.pkgs;

    otherHackagePackages =  libProf: self: super:
      with pkgs.haskell.lib; let pkg = self.callPackage; in rec {
      Agda                      = dontHaddock super.Agda;
      Diff                      = dontCheck super.Diff;
      ListLike                  = dontCheck (doJailbreak super.ListLike);
      aeson                     = dontCheck (doJailbreak super.aeson);
      base-compat-batteries     = dontCheck (doJailbreak super.base-compat-batteries);
      cabal-helper              = doJailbreak super.cabal-helper;
      cron                      = dontCheck (doJailbreak super.cron);
      diagrams-contrib          = doJailbreak super.diagrams-contrib;
      diagrams-graphviz         = doJailbreak super.diagrams-graphviz;
      diagrams-svg              = doJailbreak super.diagrams-svg;
      enclosed-exceptions       = dontCheck super.enclosed-exceptions;
      ghc-mod                   = dontCheck (doJailbreak super.ghc-mod);
      hasktags                  = dontCheck super.hasktags;
      hspec-hedgehog            = dontCheck super.hspec-hedgehog;
      hspec                     = dontCheck (doJailbreak super.hspec);
      hspec-core                = dontCheck (doJailbreak super.hspec-core);
      http-api-data             = dontCheck (doJailbreak super.http-api-data);
      jose                      = dontCheck (doJailbreak super.jose);
      lifted-base               = dontCheck (doJailbreak super.lifted-base);
      liquidhaskell             = doJailbreak super.liquidhaskell;
      pipes-binary              = doJailbreak super.pipes-binary;
      pipes-group               = doJailbreak super.pipes-group;
      pipes-zlib                = dontCheck (doJailbreak super.pipes-zlib);
      psqueues                  = dontCheck (doJailbreak super.psqueues);
      process-extras            = dontCheck (doJailbreak super.process-extras);
      semigroups                = dontCheck (doJailbreak super.semigroups);
      semigroupoids             = dontCheck (doJailbreak super.semigroupoids);
      servant                   = dontCheck (doJailbreak super.servant);
      servant-client            = dontCheck (doJailbreak super.servant-client);
      servant-client-core       = dontCheck (doJailbreak super.servant-client-core);
      servant-server            = dontCheck (doJailbreak super.servant-server);
      tasty-hspec               = dontCheck (doJailbreak super.tasty-hspec);
      text-builder              = dontCheck (doJailbreak super.text-builder);
      text-show                 = dontCheck (doJailbreak super.text-show);
      time-recurrence           = doJailbreak super.time-recurrence;
      wl-pprint-annotated       = dontCheck (doJailbreak super.wl-pprint-annotated);
    };

    otherHackagePackagesJS = libProf: self: super:
      with pkgs.haskell.lib; let pkg = self.callPackage; in rec {
      QuickCheck       = dontCheck (doJailbreak super.QuickCheck);
      aeson            = dontCheck (doJailbreak super.aeson);
      tasty-quickcheck = dontCheck super.tasty-quickcheck;
      http-types       = dontCheck super.http-types;
      comonad          = dontCheck super.comonad;
      semigroupoids    = dontCheck super.semigroupoids;
      lens             = dontCheck super.lens;
      miso             = dontCheck super.miso;
      servant          = dontCheck (doJailbreak super.servant);
      psqueues         = dontCheck super.psqueues;
    };

    ghcjs844Packages = self.haskell.packages.ghcjs.override {
      overrides = otherHackagePackagesJS false;
    };

    ghcjsEnv = self.pkgs.myEnvFun {
      name = "ghcjs";
      buildInputs = with ghcjs844Packages; [
        (ghcWithHoogle myGhcJsPackages)
      ];
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
        ghc-core
        ghcid
        hlint
        hasktags
        (self.pkgs.haskell.lib.doJailbreak stylish-haskell)
      ];
    };

     ghc82Env = self.pkgs.myEnvFun {
      name = "ghc82";
      buildInputs = with haskell822Packages; [
        (ghcWithHoogle (myHaskellPackages 8.2))
        alex happy cabal-install
        ghc-core
        ghcid
        hlint
        hasktags
        (self.pkgs.haskell.lib.doJailbreak stylish-haskell)
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
        (self.pkgs.haskell.lib.doJailbreak stylish-haskell)
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
        (self.pkgs.haskell.lib.doJailbreak stylish-haskell)
      ];
    };

    haskell863Packages = self.haskell.packages.ghc863.override {
      overrides = otherHackagePackages false;
    };

    haskell863PackagesProf = self.haskell.packages.ghc863.override {
      overrides = otherHackagePackages true;
    };

    ghc86EnvProf = self.pkgs.myEnvFun {
      name = "ghc86-prof";
      buildInputs = with haskell863PackagesProf; [
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
      buildInputs = with haskell863Packages; [
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

    coq_without_ide = self.pkgs.coq_8_8.override { buildIde = false; };
    coq88Env = with self.pkgs; self.pkgs.myEnvFun {
      name = "coq88";
      buildInputs = [
        coq_without_ide
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
        (self.pkgs.haskell.lib.doJailbreak elm)
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
