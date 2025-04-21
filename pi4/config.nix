{ config, pkgs, lib, ... }:

let
  buildGo123Module = pkgs.buildGo123Module;
  fetchFromGitHub = pkgs.fetchFromGitHub;
  k3s = pkgs.k3s;
  nixosTests = pkgs.nixosTests;
  symlinkJoin = pkgs.symlinkJoin;
in
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
    # Need to figure out what is necessary for K8s
    firewall.enable = false;
    firewall.interfaces.eth0 = {
      allowedTCPPorts = [ 22 53 6443 8443 ];
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
    # Necessary until https://github.com/NixOS/nixpkgs/pull/390981 is merged
    etcd.package = (import ../modules/etcd.nix { inherit lib symlinkJoin k3s nixosTests buildGo123Module fetchFromGitHub; });
    flannel = {
      enable = true;
      network = "10.0.0.0/16";
    };
    kubernetes = {
      apiserverAddress = "https://pi4-0:6443";
      apiserver = {
        securePort = 6443;
        advertiseAddress = "192.168.178.3";
      };
      clusterCidr = "10.0.0.0/16";
      easyCerts = true;
      masterAddress = "pi4-0";
      roles = ["master" "node"];
      addons.dns.enable = true;
      # need to point it to the arm version of the container
      addons.dns.coredns = {
          imageName = "coredns/coredns";
          imageDigest = "sha256:a0ead06651cf580044aeb0a0feba63591858fb2e43ade8c9dea45a6a89ae7e5e";
          finalImageTag = "arm64-1.10.1";
          sha256 = "sha256-IxHzSUf9g8lAmaKjcSquPKA4D6ygAz5weqKXPErHjKc=";
      };
    };
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
