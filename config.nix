{
  allowUnfree = true;

  packageOverrides = pkgs_: with pkgs_; {
    gitTools = with pkgs; buildEnv {
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

    editors = with pkgs; buildEnv {
      name = "editors";
      paths = [
        emacs25Macport
        vim
      ];
    };

    languageTools = with pkgs; buildEnv {
      name = "languageTools";
      paths = [
        cabal-install
        cabal2nix
      ];
    };

    systemPackages = with pkgs; buildEnv {
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
