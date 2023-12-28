{ config, lib, pkgs, darwin, emacs, phps, ... }:

{
  imports = [
    ../modules/tmux.nix
  ];

  disabledModules = [ "targets/darwin/linkapps.nix" ];
  documentation = {
    enable = false;
  };

  environment = {
    systemPackages = [ ];
    variables.LANG = "en_US.UTF-8";
    variables.LC_ALL = "en_US.UTF-8";
  };

  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      fira-code
      jetbrains-mono
      monaspace
      nerdfonts
      victor-mono
    ];
  };

  nix = {
    configureBuildUsers = true;
    extraOptions = ''
      gc-keep-derivations = true
      gc-keep-outputs = true
      keep-outputs = true
      keep-derivations = true
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    package = pkgs.nixVersions.stable;
    settings = {
      cores = 6;
      experimental-features = [ "nix-command" "flakes" ];
      max-jobs = 6;
      trusted-public-keys = [
      ];
      substituters = [
      ];
      trusted-substituters = [
      ];
      trusted-users = [
        "sebastian"
      ];
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
    config.allowBroken = true;
    config.allowUnsupportedSystem = true;
    overlays = [
      emacs.overlay
      phps.overlays.default
      (import ../overlays/packages.nix)
    ];
  };

  users.users = {
    sebastian = {
      home = "/Users/sebastian";
    };
  };

  networking = {
    hostName = "monad";
    knownNetworkServices = [ "Wi-Fi" ];
  };

  programs.zsh.enable = true;

  services = {
    emacs = {
      enable = false;
      package = (import ../modules/emacs/emacs.nix { inherit pkgs; });
    };
    nix-daemon = {
      enable = true;
    };
    # sketchybar = {
    #   enable = true;
    # };
  };

  security = {
    pam = {
      enableSudoTouchIdAuth = true;
    };
  };

  system = {
    activationScripts.applications.text = pkgs.lib.mkForce (
      ''
        echo "setting up ~/Applications..." >&2
        rm -rf ~/Applications/Nix\ Apps
        mkdir -p ~/Applications/Nix\ Apps
        for app in $(find ${config.system.build.applications}/Applications -maxdepth 1 -type l); do
          src="$(/usr/bin/stat -f%Y "$app")"
          cp -r "$src" ~/Applications/Nix\ Apps
        done
        chown sebastian ~/Applications -R
      ''
    );

    defaults = {
      NSGlobalDomain = {
        AppleInterfaceStyle = "Dark";
        AppleInterfaceStyleSwitchesAutomatically = false;
        AppleKeyboardUIMode = 3;
        AppleMeasurementUnits = "Centimeters";
        AppleMetricUnits = 1;
        ApplePressAndHoldEnabled = false;
        AppleShowScrollBars = "WhenScrolling";
        AppleTemperatureUnit = "Celsius";
        InitialKeyRepeat = 15;
        KeyRepeat = 2;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        NSDocumentSaveNewDocumentsToCloud = true;
        NSTableViewDefaultSizeMode = 2;
        "com.apple.sound.beep.feedback" = 0;
      };

      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;

      alf = {
        allowdownloadsignedenabled = 1;
        allowsignedenabled = 1;
        globalstate = 1;
        loggingenabled = 1;
      };

      dock = {
        autohide = true;
        orientation = "bottom";
        showhidden = true;
        mru-spaces = false;
      };

      finder = {
        AppleShowAllExtensions = true;
        FXEnableExtensionChangeWarning = false;
        QuitMenuItem = true;
        ShowPathbar = true;
        ShowStatusBar = true;
      };

      trackpad = {
        Clicking = true;
        TrackpadThreeFingerDrag = true;
      };
    };

    keyboard = {
      enableKeyMapping = true;
    };

    stateVersion = 4;
  };
}
