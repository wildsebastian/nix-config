{ config, pkgs, ... }:

{
  programs.zsh.enable = true;
  programs.zsh.enableBashCompletion = true;
  programs.zsh.enableSyntaxHighlighting = true;
  programs.zsh.promptInit = ''
    eval "$(starship init zsh)"

    if [ -n "$\{commands[fzf-share]\}" ]; then
      source "$(fzf-share)/key-bindings.zsh"
      source "$(fzf-share)/completion.zsh"
    fi
  '';

  programs.zsh.loginShellInit = ''
    :r() {
      direnv reload
    }

    eval "$(direnv hook zsh)"

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

  environment.etc."zshenv".text = ''
    if [ -d "$HOME/.nixpkgs/bin" ] ; then
      PATH="$HOME/.nixpkgs/bin:$PATH"
    fi
  '';

  environment.shellAliases = {
    cat         = "bat";
    drs         = "darwin-rebuild switch";
    du          = "dust";
    em          = "nohup emacs & disown";
    find        = "fd";
    gf          = "git fetch";
    gg          = "git log --color --graph";
    gl          = "git log --color -32";
    grbm        = "git rebase origin/master";
    gst         = "git status";
    ls          = "exa";
    nixs        = "nix-env -f '<nixpkgs>' -qaP | grep";
    ps          = "procs";
    wget        = "wget2";
  };
}
