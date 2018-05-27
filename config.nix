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
  };
}
