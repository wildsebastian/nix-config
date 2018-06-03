{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in rec {

    ghc82Env = self.pkgs.myEnvFun {
      name = "ghc82";
      buildInputs = with self.haskell.packages.ghc822; [
        self.haskell.compiler.ghc822
        alex happy cabal-install
        ghc-core
        ghcid
        hlint
        (self.haskell.lib.dontCheck hasktags)
      ];
    };

    ghc84Env = self.pkgs.myEnvFun {
      name = "ghc84";
      buildInputs = with self.haskell.packages.ghc843; [
        self.haskell.compiler.ghc843
        alex happy cabal-install
        ghc-core
        ghcid
        hlint
        (self.haskell.lib.dontCheck hasktags)
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
