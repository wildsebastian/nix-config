{ config, lib, pkgs, ... }:

{
  disabledModules = [ "targets/darwin/linkapps.nix" ];

  home = {
    username = "sebastian";
    stateVersion = "22.11";
    packages = import ./packages.nix { inherit pkgs; };

    activation = lib.mkIf pkgs.stdenv.isDarwin {
      copyApplications =
        let
          apps = pkgs.buildEnv {
            name = "home-manager-applications";
            paths = config.home.packages;
            pathsToLink = "/Applications";
          };
        in
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          baseDir="$HOME/Applications/Home Manager Apps"
          if [ -d "$baseDir" ]; then
            rm -rf "$baseDir"
          fi
          mkdir -p "$baseDir"
          for appFile in ${apps}/Applications/*; do
            target="$baseDir/$(basename "$appFile")"
            $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
            $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
          done
        '';
    };
  };

  # Let Home Manager install and manage itself.
  programs = {
    alacritty = {
      enable = true;
      settings = {
        scrolling.history = 10000;
        font = {
          normal = {
            family = "Iosevka Nerd Font";
            style = "Regular";
          };
          bold = {
            family = "Iosevka Nerd Font";
            style = "Bold";
          };
          italic = {
            family = "Iosevka Nerd Font";
            style = "Italic";
          };
          bold_italic = {
            family = "Iosevka Nerd Font";
            style = "Bold Italic";
          };
          size = 14.0;
        };
        colors = {
          primary = {
            background = "#2e3440";
            foreground = "#d8dee9";
            dim_foreground = "#a5abb6";
          };
          cursor = {
            text = "#2e3440";
            cursor = "#d8dee9";
          };
          vi_mode_cursor = {
            text = "#2e3440";
            cursor = "#d8dee9";
          };
          selection = {
            text = "CellForeground";
            background = "#4c566a";
          };
          search = {
            matches = {
              foreground = "CellBackground";
              background = "#88c0d0";
            };
            footer_bar = {
              background = "#434c5e";
              foreground = "#d8dee9";
            };
          };
          normal = {
            black = "#3b4252";
            red = "#bf616a";
            green = "#a3be8c";
            yellow = "#ebcb8b";
            blue = "#81a1c1";
            magenta = "#b48ead";
            cyan = "#88c0d0";
            white = "#e5e9f0";
          };
          bright = {
            black = "#4c566a";
            red = "#bf616a";
            green = "#a3be8c";
            yellow = "#ebcb8b";
            blue = "#81a1c1";
            magenta = "#b48ead";
            cyan = "#8fbcbb";
            white = "#eceff4";
          };
          dim = {
            black = "#373e4d";
            red = "#94545d";
            green = "#809575";
            yellow = "#b29e75";
            blue = "#68809a";
            magenta = "#8c738c";
            cyan = "#6d96a5";
            white = "#aeb3bb";
          };
        };
        selection = {
          semantic_escape_chars = ",│`|:\"' ()[]{}<>\t";
        };

        cursor = {
          style = {
            shape = "Underline";
            blinking = "On";
          };
          vi_mode_style = "None";
          blink_interval = 750;
          blink_timeout = 5;
          unfocused_hollow = true;
          thickness = 0.15;
        };
        live_config_reload = true;
      };
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      package = (pkgs.gitAndTools.gitFull.override { sendEmailSupport = true; });
      delta = {
        enable = true;
        options = {
          navigate = true;
          light = false;
          side-by-side = true;
          line-numbers = true;
        };
      };
      extraConfig = {
        core = {
          editor = "nvim";
        };
        diff.colorMoved = "default";
        gpg.format = "ssh";
        "gpg \"ssh\"".program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
        init.defaultBranch = "main";
        merge.conflictstyle = "diff3";
        user.signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDdFv1ZT9EzT2mrapiucBoe83vJDwRuBri245aYL+dmI";
      };
      userEmail = "sebastian@wild-siena.com";
      userName = "Sebastian Wild";
    };

    home-manager.enable = true;

    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
    };

    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        aws.symbol = "  ";
        buf.symbol = " ";
        c.symbol = " ";
        conda.symbol = " ";
        dart.symbol = " ";
        directory.read_only = " ";
        docker_context.symbol = " ";
        elixir.symbol = " ";
        elm.symbol = " ";
        git_branch.symbol = " ";
        golang.symbol = " ";
        haskell.symbol = " ";
        hg_branch.symbol = " ";
        java.symbol = " ";
        julia.symbol = " ";
        lua.symbol = " ";
        memory_usage.symbol = " ";
        nim.symbol = " ";
        nix_shell.symbol = " ";
        nodejs.symbol = " ";
        package.symbol = " ";
        purescript.symbol = " ";
        python.symbol = " ";
        rlang.symbol = "ﳒ ";
        ruby.symbol = " ";
        rust.symbol = " ";
        spack.symbol = "🅢 ";
      };
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      autocd = true;
      envExtra = ''
        if [ -d "$HOME/.nixpkgs/bin" ] ; then
          PATH="$HOME/.nixpkgs/bin:$PATH"
        fi

        if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
          source /etc/zprofile
        fi
      '';
      history = {
        expireDuplicatesFirst = true;
        ignoreDups = true;
        save = 10000;
        share = true;
        size = 10000;
      };
      initExtra = ''
        eval "$(starship init zsh)"
        eval "$(direnv hook zsh)"
      '';
      loginExtra = ''
        :r() {
          direnv reload
        }

        function gi() {
          curl -sL https://www.gitignore.io/api/$@
        }
      '';
      shellAliases = {
        cat = "bat";
        du = "dust";
        ec = "nohup emacsclient -c & disown";
        em = "nohup emacs & disown";
        gf = "git fetch";
        gg = "git log --color --graph";
        gl = "git log --color -32";
        grbm = "git rebase origin/master";
        gst = "git status";
        ls = "exa";
        ps = "procs";
        vim = "nvim";
        wget = "wget2";
      };
    };
  };
}
