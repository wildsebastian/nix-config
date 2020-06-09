{ config, lib, pkgs, ... }:

let
  localconfig = import ./local.nix;
in
{
  imports = [
    ./tmux.nix
    ./vim.nix
    ./zsh.nix
  ] ++ (if localconfig.hostname == "Nixpkgs" then [
    ./postgres.nix
    ./redis.nix
  ] else []);

  system.defaults.NSGlobalDomain.AppleShowScrollBars = "WhenScrolling";
  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 15;
  system.defaults.NSGlobalDomain.KeyRepeat = 2;
  system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticPeriodSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;

  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "bottom";
  system.defaults.dock.showhidden = true;
  system.defaults.dock.mru-spaces = false;

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.QuitMenuItem = true;
  system.defaults.finder.FXEnableExtensionChangeWarning = false;

  system.defaults.trackpad.Clicking = true;
  system.defaults.trackpad.TrackpadThreeFingerDrag = true;

  system.keyboard.enableKeyMapping = true;

  environment.systemPackages = import ./packages.nix { inherit pkgs; inherit config; };

  nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;

  environment.variables.LANG = "en_US.UTF-8";
  environment.variables.LC_ALL = "en_US.UTF-8";
  environment.darwinConfig = "$HOME/.nixpkgs/darwin-configuration.nix";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 3;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 2;
  nix.buildCores = 2;

  nix.extraOptions = ''
    gc-keep-derivations = true
    gc-keep-outputs = true
  '';

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  nixpkgs.config.allowUnsupportedSystem = false;
  nixpkgs.overlays = [
    (import ./overlays/haskell.nix)
    (import ./overlays/vim.nix)
  ];

  nix.nixPath = [
    "darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
    "nixpkgs=$HOME/.nix-defexpr/nixpkgs"
    "darwin=$HOME/.nix-defexpr/darwin"
  ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = false;

  services.emacs.enable = false;
  services.emacs.package = (import ./emacs.nix { inherit pkgs; });
}
