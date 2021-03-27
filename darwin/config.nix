{ config, lib, pkgs, ... }:

{
  nix = {
    binaryCachePublicKeys = [ "nix-cache:voABK2fsjF4bXgwIt2+iFymykxhT0Gk60XWAs7uoP+Y=" ];
    binaryCaches = [ s3://nix-cache?profile=nix-cache&scheme=https&endpoint=fra1.digitaloceanspaces.com ];
    trustedBinaryCaches = [ s3://nix-cache?profile=nix-cache&scheme=https&endpoint=fra1.digitaloceanspaces.com ];
    trustedUsers = [ "@admin" "@sebastian" ];
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

  # Auto upgrade nix package and the daemon service.
  users.nix.configureBuildUsers = true;
}
