{ config, pkgs, lib, ... }:

{
  imports = [
    ../modules/tmux.nix
    ../modules/zsh.nix
  ];

  boot = {
    kernelModules = [ "cgroup" ];
    kernelPackages = pkgs.linuxPackages_rpi4;
    kernelParams = [
      "cgroup_enable=memory"
      "systemd.unified_cgroup_hierarchy=1"
    ];
  };

  nix = {
    settings = {
      max-jobs = 2;
      cores = 4;
      require-sigs = true;
      experimental-features = [ "nix-command" "flakes" ];
    };
    package = pkgs.nix;
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
    extraOptions = ''
      gc-keep-derivations = true
      gc-keep-outputs = true
    '';
  };

  nixpkgs = {
    config.allowUnfree = true;
    config.allowBroken = true;
    config.allowUnsupportedSystem = true;
  };

  environment = {
    systemPackages = import ./packages.nix { inherit pkgs; };
    variables.LANG = "en_US.UTF-8";
    variables.LC_ALL = "en_US.UTF-8";
  };

  time.timeZone = "UTC";

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  networking = {
    hostName = "pi4-0";
    wireless = {
      enable = false;
    };
    interfaces.eth0.ipv4.addresses = [{
      address = "192.168.178.3";
      prefixLength = 24;
    }];
    defaultGateway = "192.168.178.1";
    nameservers = [ "192.168.178.3" "1.1.1.1" ];
    firewall.interfaces.eth0 = {
      allowedTCPPorts = [ 22 53 6443 ];
      allowedUDPPorts = [ 53 ];
    };

    extraHosts = "192.168.178.3 pi4-0";
  };

  system.stateVersion = "25.05";

  programs = {
    mosh.enable = true;
    zsh = {
      enable = true;
      histSize = 10000;
    };
  };
  services = {
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        AllowUsers = ["sebastian" "root"];
        UseDns = true;
        X11Forwarding = false;
        PermitRootLogin = "prohibit-password";
      };
    };
    pulseaudio.enable = false;
  };

  users = {
    users = {
      root = {
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDdFv1ZT9EzT2mrapiucBoe83vJDwRuBri245aYL+dmI sebastian@monad"
        ];
        shell = pkgs.zsh;
      };

      sebastian = {
        isNormalUser = true;
        extraGroups = [ "wheel" "docker" ];
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDdFv1ZT9EzT2mrapiucBoe83vJDwRuBri245aYL+dmI sebastian@monad"
        ];
        shell = pkgs.zsh;
      };
    };
  };
}
