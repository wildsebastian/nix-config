{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableBashCompletion = true;
    syntaxHighlighting.enable = true;
    promptInit = ''
      eval "$(starship init zsh)"
    '';

    loginShellInit = ''
      :r() {
        direnv reload
      }

      function gi() {
        curl -sL https://www.gitignore.io/api/$@
      }

      gfcto() {
        git fetch && git checkout -b $1 origin/$1
      }

      gfctr() {
        git fetch && git checkout -b $2 $1/$2
      }

      nixhs() {
        nix-env -f '<nixpkgs>' -qaP -A haskell.packages.$1 | rg $2;
      }

      nixpy() {
        nix-env -f '<nixpkgs>' -qaP -A python$1Packages | rg $2;
      }

      nixel() {
        nix-env -f '<nixpkgs>' -qaP -A emacsPackages | rg $1;
      }
    '';
  };

  environment.etc."zshenv".text = ''
    if [ -d "$HOME/src/nix-config/bin" ] ; then
      PATH="$HOME/src/nix-config/bin:$PATH"
    fi

    if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
      source /etc/zprofile
    fi
  '';

  environment.shellAliases = {
    cat = "bat";
    drs = "darwin-rebuild switch";
    du = "dust";
    ec = "nohup emacsclient -c & disown";
    em = "nohup emacs & disown";
    gf = "git fetch";
    gg = "git log --color --graph";
    gl = "git log --color -32";
    grbm = "git rebase origin/master";
    gst = "git status";
    ls = "eza";
    nixs = "nix-env -f '<nixpkgs>' -qaP | grep";
    ps = "procs";
    vim = "nvim";
    wget = "wget2";
  };
}
