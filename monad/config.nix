{ config, lib, pkgs, darwin, emacs, ... }:

{
  imports = [
    ../modules/tmux.nix
    # ../modules/zsh.nix
    ../services/hoogle.nix
  ];

  environment = {
    systemPackages = import ./packages.nix { inherit pkgs; };
    variables.LANG = "en_US.UTF-8";
    variables.LC_ALL = "en_US.UTF-8";
    pathsToLink = [
      "/share/nix-direnv"
    ];
  };

  fonts = {
    fontDir.enable = true;
    fonts = [
      pkgs.jetbrains-mono
      pkgs.nerdfonts
      pkgs.sarasa-gothic
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
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
    config.allowBroken = true;
    config.allowUnsupportedSystem = true;
    overlays = [
      emacs.overlay
      (self: super: {
        nix-direnv = super.nix-direnv.override { enableFlakes = true; };
      })
      (import ../overlays/haskell.nix)
      (import ../overlays/packages.nix)
    ];
  };

  networking = {
    hostName = "monad";
    knownNetworkServices = [ "Wi-Fi" ];
  };

  programs.zsh.enable = true;

  services = {
    emacs = {
      enable = true;
      package = (import ../modules/emacs/emacs.nix { inherit pkgs; });
    };
    hoogle = {
      enable = true;
      port = 8100;
      home = "http://localhost:8100";
      packages = p: [
        p.aeson
        p.base64-bytestring
        p.bytestring
        p.case-insensitive
        p.chronos
        p.cron
        p.co-log
        p.containers
        p.cryptonite
        p.dhall
        p.directory
        p.filepath
        p.generics-sop
        p.hakyll
        p.hedgehog
        p.http-client
        p.http-client-tls
        p.http-types
        p.jose
        p.lens
        p.memory
        p.mtl
        p.optparse-applicative
        p.pandoc
        p.primitive
        p.shelly
        p.servant
        p.servant-auth
        p.servant-auth-server
        p.servant-client
        p.servant-server
        p.squeal-postgresql
        p.tasty
        p.tasty-hedgehog
        p.tasty-hspec
        p.tasty-test-reporter
        p.text
        p.time
        p.transformers
        p.unliftio-core
        p.unordered-containers
        p.vector
        p.wai
        p.warp
      ];
    };
    nix-daemon = {
      enable = true;
    };
  };

  system = {
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
