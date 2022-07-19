{ config, pkgs, lib, ... }:

let
  localconfig = import ./local.nix;
in

{
  imports = [
    ./tmux.nix
    ./zsh.nix
    ./services/hoogle.nix
  ] ++ (if localconfig.system == "Darwin" then
  [ ./darwin/config.nix ] else
  [ ./pi4/config.nix ]);

  nix = {
    package = pkgs.nix;
    maxJobs = 2;
    buildCores = 2;
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    extraOptions = ''
      gc-keep-derivations = true
      gc-keep-outputs = true
    '';
    nixPath = [
      "nixpkgs=$HOME/.nix-defexpr/nixpkgs"
    ] ++ (if localconfig.system == "Darwin" then [
      "darwin-config=$HOME/.nixpkgs/configuration.nix"
      "darwin=$HOME/.nix-defexpr/darwin"
    ] else [
      "nixos-config=/etc/nixos/configuration.nix"
    ]);
    requireSignedBinaryCaches = true;
  };

  nixpkgs = {
    config.allowUnfree = true;
    config.allowBroken = true;
    config.allowUnsupportedSystem = true;
    overlays = [
      (import ./overlays/emacs-overlay/default.nix)
      (import ./overlays/haskell.nix)
      (import ./overlays/packages.nix)
      (import ./overlays/python.nix)
    ];
  };

  environment = {
    systemPackages = import ./packages.nix { inherit pkgs; };
    variables.LANG = "en_US.UTF-8";
    variables.LC_ALL = "en_US.UTF-8";
  };

  fonts = {
    fontDir.enable = true;
    fonts = [
      pkgs.jetbrains-mono
      pkgs.nerdfonts
      pkgs.iosevka
      pkgs.sarasa-gothic
    ];
  };

  time.timeZone = "Europe/Berlin";

  services = {
    emacs = {
      enable = true;
      package = (import ./emacs.nix { inherit pkgs; });
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
  };
}
