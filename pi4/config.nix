{ config, pkgs, lib, ... }:

  let
    user = "sebastian";
    passwordHash = "$6$1OkN.2NSh$oGkKmBKy8rhND5Q/rE7.WYWOWHx.BUR61lxEomMQHZ0Q1EmOYzpaWZ6ilr9q8kuE289zHwvddEy.4Y.IvwXu51";
    SSID = "Sicilia";
    SSIDpassword = "naEN6XXe68^k{83CieC=%%ybstxVah>rVwqufhJtqXf$4D%p=9dVL7Qx(hB7wp3";
    interface = "wlan0";
    hostname = "rpi4-0";
  in {
    imports = ["${fetchTarball "https://github.com/NixOS/nixos-hardware/archive/936e4649098d6a5e0762058cb7687be1b2d90550.tar.gz" }/raspberry-pi/4"];

    fileSystems = {
      "/" = {
        device = "/dev/disk/by-label/NIXOS_SD";
        fsType = "ext4";
        options = [ "noatime" ];
      };
    };

    networking = {
      hostName = hostname;
      wireless = {
        enable = true;
        networks."${SSID}".psk = SSIDpassword;
        interfaces = [ interface ];
      };
    };

    environment.systemPackages = with pkgs; [ git starship vim ];

    nix = {
      autoOptimiseStore = true;
      # Free up to 1GiB whenever there is less than 100MiB left.
      extraOptions = ''
        min-free = ${toString (100 * 1024 * 1024)}
        max-free = ${toString (1024 * 1024 * 1024)}
      '';
    };

    powerManagement.cpuFreqGovernor = "ondemand";

    services = {
      openssh = {
        enable = true;
      };
    };

    users = {
      mutableUsers = false;
      users."${user}" = {
        isNormalUser = true;
        hashedPassword = passwordHash;
        extraGroups = [ "wheel" "docker" ];
        openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC5ivHP+Az4mJC2eKpV5ALZFnoa1bMCr2CEY6r9ErM6nlGLZ159ec9HGf06IHXvk0kFmtrWeaT4CKY5LdZubHyTnIzeGgDRX8J47qcKl2NG0dScVebX/EF7kXIA15t8ek6hbPvkXNq0ORYWyaIfALr17vxMZ8oET+lBadNGZBTm67pHno0y/kS5nS2lei0izR2rO0m/an5yZ7DXJzuzx8VVL1OaYgHpa9Mv1lviyuIWnQdDG/ohTPVIEkDMqAQoUzzEDUUgR78pHgyRR+QFkA6JCe+w/V9rKvpGHQ2dV8IImyWniX/V7bupmSKdSBES/bUzhC2scBhwVvKq/XJ9lNet sebastian@desktop" ];
        defaultUserShell = pkgs.zsh;
      };
    };

    # Enable GPU acceleration
    hardware.raspberry-pi."4".fkms-3d.enable = true;

    hardware.pulseaudio.enable = true;

    virtualisation.docker.enable = true;
  }
