{ config, lib, pkgs, ... }:

{
  nix = {
    settings = {
      trusted-substituters = [ s3://nix-cache?profile=nix-cache&scheme=https&endpoint=fra1.digitaloceanspaces.com ];
      trusted-users = [ "@admin" "@sebastian" ];
      trusted-public-keys = [ "nix-cache:voABK2fsjF4bXgwIt2+iFymykxhT0Gk60XWAs7uoP+Y=" ];
      require-sigs = true;
    };
    configureBuildUsers = true;
  };

  networking = {
    hostName = "monad";
    knownNetworkServices = [ "Wi-Fi" ];
  };

  services = {
    nix-daemon = {
      enable = true;
    };
  };

  system = {
    defaults = {
      NSGlobalDomain = {
        AppleShowScrollBars = "WhenScrolling";
        AppleKeyboardUIMode = 3;
        ApplePressAndHoldEnabled = false;
        InitialKeyRepeat = 15;
        KeyRepeat = 2;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
      };

      dock = {
        autohide = true;
        orientation = "bottom";
        showhidden = true;
        mru-spaces = false;
      };

      finder = {
        AppleShowAllExtensions = true;
        QuitMenuItem = true;
        FXEnableExtensionChangeWarning = false;
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
