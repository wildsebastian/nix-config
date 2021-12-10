{ config, pkgs, lib, ... }:

let
  localconfig = import ./local.nix;
in

{
  imports = [
    ./tmux.nix
    ./zsh.nix
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
    enableFontDir = true;
    fonts = [ pkgs.iosevka pkgs.nerdfonts ];
  };

  time.timeZone = "Europe/Berlin";
}
