{ config, pkgs, lib, ... }:

{
  # This configuration worked on 09-03-2021 nixos-unstable @ commit 102eb68ceec
  # The image used https://hydra.nixos.org/build/134720986
  boot = {
    kernelPackages = pkgs.linuxPackages_rpi4;
  };

  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
  };
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # Required for the Wireless firmware
  hardware.enableRedistributableFirmware = true;

  networking = {
    hostName = "nixpi"; # Define your hostname.
    networkmanager = {
      enable = true;
    };
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = true;
    users.wilds = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };
  };

  nix = {
    autoOptimiseStore = true;
    # Free up to 1GiB whenever there is less than 100MiB left.
    extraOptions = ''
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
    '';
  };

  # Assuming this is installed on top of the disk image.
  fileSystems = {
    "/boot/firmware" = {
      device = "/dev/disk/by-label/FIRMWARE";
      fsType = "vfat";
    };

    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  powerManagement.cpuFreqGovernor = "ondemand";

  services = {
    openssh = {
      enable = true;
      forwardX11 = true;
    };
  };
}
