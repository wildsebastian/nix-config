{ config, pkgs, ... }:

{
  programs.zsh.enable = true;
  programs.zsh.enableBashCompletion = true;
  programs.zsh.enableFzfCompletion = true;
  programs.zsh.enableFzfGit = true;
  programs.zsh.enableFzfHistory = true;
  programs.zsh.enableSyntaxHighlighting = true;
  programs.zsh.variables.cfg = "$HOME/.nixpkgs/darwin-configuration.nix";
  programs.zsh.variables.darwin = "$HOME/.nix-defexpr/darwin";
  programs.zsh.variables.nixpkgs = "$HOME/.nix-defexpr/nixpkgs";

  programs.zsh.promptInit = ''
    autoload -U promptinit && promptinit
    setopt PROMPTSUBST

    LAMBDA=$'\u03BB'
    ARROW=$'\uF105'
    GITLAB=$'\uF296'
    GITHUB=$'\uF09B'
    GIT=$'\uF7A1'
    GITBRANCH=$'\uF418'
    CHECK=$'\uF058'
    CROSS=$'\uF06A'
    LINEUP=$'\e[1A'
    LINEDOWN=$'\e[1B'

    gitstatus() {
      if git rev-parse --git-dir > /dev/null 2>&1; then
        setopt promptsubst
        autoload -Uz vcs_info

        zstyle ':vcs_info:*' enable git
        zstyle ':vcs_info:*' get-revision true
        zstyle ':vcs_info:*' check-for-changes true
        zstyle ':vcs_info:*' stagedstr '\u271A'
        zstyle ':vcs_info:*' unstagedstr '\u272A'
        zstyle ':vcs_info:*' formats ' %b \uF418 %u%c '
        zstyle ':vcs_info:*' actionformats ' %b \uF418 %u%c '
        vcs_info
        if [ -z "$(git status -s)" ]; then
          echo -n "%F{green}$vcs_info_msg_0_%f"
        else
          echo -n "%F{yellow}$vcs_info_msg_0_%f"
        fi
      else
        echo -n ""
      fi;
    }

    arrow() {
      echo -n "%{%F{green}%}$ARROW%{%f%}"
    }

    prompt_date() {
      echo -n "%{%F{white}%} %D{%a %d.%m.%Y %H:%M:%S}%{%f%}"
    }

    lambda() {
      echo -n "%{%F{green}%}$LAMBDA%{%f%}"
    }

    path() {
      echo -n "%{%F{blue}%}%2~%{%f%}"
    }

    exitcode() {
      echo -n "%(?.%{%F{green}%} %? $CHECK %{%f%}.%{%F{red}%} %? $CROSS %{%f%})"
    }

    RPROMPT_PREFIX='%{'$LINEUP'%}' # one line up
    RPROMPT_SUFFIX='%{'$LINEDOWN'%}' # one line down

    PROMPT='$(path) $(gitstatus)
    $(lambda) %m $(arrow) '
    RPROMPT=$RPROMPT_PREFIX'$(prompt_date) $(exitcode)'$RPROMPT_SUFFIX
  '';

  programs.zsh.loginShellInit = ''
    :r() {
      direnv reload
    }

    eval "$(direnv hook zsh)"

    function gi() {
      curl -sL https://www.gitignore.io/api/$@
    }

    function vterm_printf() {
      if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
      elif [ ''${TERM%%-*} = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
      else
        printf "\e]%s\e\\" "$1"
      fi
    }

    gfcto() {
      git fetch && git checkout -b $1 origin/$1
    }

    gfctr() {
      git fetch && git checkout -b $2 $1/$2
    }
  '';

  environment.etc."zshenv".text = ''
    if [ -d "$HOME/.nixpkgs/bin" ] ; then
      PATH="$HOME/.nixpkgs/bin:$PATH"
    fi
  '';

  environment.shellAliases = {
    nixs        = "nix-env -f '<nixpkgs>' -qaP | grep";
    nixhs       = "nix-env -f '<nixpkgs>' -qaP -A haskellPackages | grep";
    drs         = "darwin-rebuild switch";
    gf          = "git fetch";
    gg          = "git log --color --graph";
    gl          = "git log --color -32";
    grbm        = "git rebase origin/master";
    gst         = "git status";

    ed          = "nohup emacs --bg-daemon=main >/dev/null";
    ec          = "emacsclient -c -s main &";
    ek          = "killall -9 emacs";
  };
}
