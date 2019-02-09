{ pkgs }: {
  allowUnfree = true;
  allowBroken = true;

  packageOverrides = super: let
    self = super.pkgs;
    myHaskellPackages = import ./haskell.nix self.pkgs; in rec {
    myGhcJsPackages = import ./ghcjs.nix self.pkgs;

    otherHackagePackages =  libProf: self: super:
      with pkgs.haskell.lib; let pkg = self.callPackage; in rec {
      Agda                      = dontHaddock super.Agda;
      Diff                      = dontCheck super.Diff;
      ListLike                  = dontCheck (doJailbreak super.ListLike);
      aeson                     = dontCheck (doJailbreak (addBuildDepend super.aeson self.contravariant));
      base-compat-batteries     = dontCheck (doJailbreak (addBuildDepend super.base-compat-batteries self.contravariant));
      cabal-helper              = doJailbreak super.cabal-helper;
      cereal                    = dontCheck super.cereal;
      cron                      = dontCheck (doJailbreak super.cron);
      diagrams-contrib          = doJailbreak super.diagrams-contrib;
      diagrams-graphviz         = doJailbreak super.diagrams-graphviz;
      diagrams-svg              = doJailbreak super.diagrams-svg;
      enclosed-exceptions       = dontCheck super.enclosed-exceptions;
      ghc-mod                   = dontCheck (doJailbreak super.ghc-mod);
      hasktags                  = dontCheck super.hasktags;
      hpack_0_31_1              = super.hpack;
      hspec-hedgehog            = dontCheck super.hspec-hedgehog;
      hspec                     = dontCheck (doJailbreak super.hspec);
      hspec-core                = dontCheck (doJailbreak super.hspec-core);
      http-api-data             = dontCheck (doJailbreak super.http-api-data);
      http-conduit              = dontCheck (doJailbreak super.http-conduit);
      jose                      = dontCheck (doJailbreak super.jose);
      lifted-base               = dontCheck (doJailbreak super.lifted-base);
      liquidhaskell             = doJailbreak super.liquidhaskell;
      pipes-binary              = doJailbreak super.pipes-binary;
      pipes-group               = doJailbreak super.pipes-group;
      pipes-zlib                = dontCheck (doJailbreak super.pipes-zlib);
      postgresql-simple         = dontCheck (doJailbreak super.postgresql-simple);
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
      tls                       = dontCheck (doJailbreak super.tls);
      time-recurrence           = doJailbreak super.time-recurrence;
      wl-pprint-annotated       = dontCheck (doJailbreak super.wl-pprint-annotated);
      yaml_0_11_0_0             = super.yaml;
    };

    otherHackagePackagesJS = libProf: self: super:
      with pkgs.haskell.lib; let pkg = self.callPackage; in rec {
      QuickCheck       = dontCheck (doJailbreak super.QuickCheck);
      aeson            = dontCheck (doJailbreak super.aeson);
      cereal           = dontCheck super.cereal;
      tasty-quickcheck = dontCheck super.tasty-quickcheck;
      http-types       = dontCheck super.http-types;
      comonad          = dontCheck super.comonad;
      conduit-extra    = dontCheck super.conduit-extra;
      semigroupoids    = dontCheck super.semigroupoids;
      lens             = dontCheck super.lens;
      miso             = dontCheck super.miso;
      servant          = dontCheck (doJailbreak super.servant);
      psqueues         = dontCheck (doJailbreak super.psqueues);
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
        mercurialFull
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
        jq
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
