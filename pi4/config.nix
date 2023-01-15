{ config, pkgs, lib, ... }:

{
  imports = [
    ../modules/tmux.nix
    ../modules/zsh.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_rpi4;

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

  time.timeZone = "Europe/Berlin";

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
      allowedTCPPorts = [ 22 53 80 3000 8000 2375 2379 2380 5432 6379 6443 ];
      allowedUDPPorts = [ 53 67 ];
      allowedUDPPortRanges = [{ from = 60000; to = 61000; }];
    };
  };

  system.stateVersion = "22.05";

  programs = {
    mosh.enable = true;
    zsh = {
      enable = true;
      histSize = 10000;
    };
  };
  services = {
    nginx = {
      enable = true;
      virtualHosts."192.168.178.3" = {
        enableACME = false;
        enableSSL = false;
        locations."/" = {
          proxyPass = "http://192.168.178.3:3000";
        };
        locations."/pihole/" = {
          proxyPass = "http://192.168.178.3:8000/admin/";
        };
      };
    };
    grafana = {
      dataDir = "/var/lib/grafana";
      enable = true;
      settings = {
        server = {
          http_addr = "192.168.178.3";
          http_port = 3000;
          domain = "192.168.178.3";
          protocol = "http";
        };
      };
    };
    openssh.enable = true;
    prometheus = {
      enable = true;
      port = 9001;
      exporters = {
        node = {
          enable = true;
          port = 9002;
          enabledCollectors = [ "netdev" ];
        };
        blackbox = {
          enable = true;
          enableConfigCheck = true;
          port = 9003;
          configFile = ./blackbox.yaml;
        };
        postgres = {
          enable = true;
          port = 9004;
        };
        redis = {
          enable = true;
          port = 9005;
        };
        pihole = {
          enable = true;
          port = 9006;
          apiToken = "365d84d2a47d3c42c8a261973d5c53e652113e9f88b4bcfb07439c62da333559";
          piholeHostname = "127.0.0.1";
          piholePort = 8000;
        };
      };
      scrapeConfigs = [{
        job_name = "node";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
        }];
        scrape_interval = "15s";
      }
        {
          job_name = "blackbox";
          params.module = [ "icmp_ipv4" ];
          metrics_path = "/probe";
          scrape_interval = "15s";
          static_configs = [{
            targets = [ "1.1.1.2" "1.0.0.2" ];
          }];
          relabel_configs = [{
            source_labels = [ "__address__" ];
            target_label = "__param_target";
          }
            {
              source_labels = [ "__param_target" ];
              target_label = "instance";
            }
            {
              target_label = "__address__";
              replacement = "127.0.0.1:9003";
            }];
        }
        {
          job_name = "blackbox-http";
          params.module = [ "http_2xx" ];
          metrics_path = "/probe";
          scrape_interval = "15s";
          static_configs = [{
            targets = [ "https://wildsebastian.eu" ];
          }];
          relabel_configs = [{
            source_labels = [ "__address__" ];
            target_label = "__param_target";
          }
            {
              source_labels = [ "__param_target" ];
              target_label = "instance";
            }
            {
              target_label = "__address__";
              replacement = "127.0.0.1:9003";
            }];
        }
        {
          job_name = "postgres";
          static_configs = [{
            targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.postgres.port}" ];
          }];
          scrape_interval = "15s";
        }
        {
          job_name = "redis";
          static_configs = [{
            targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.redis.port}" ];
          }];
          scrape_interval = "15s";
        }
        {
          job_name = "cadvisor";
          static_configs = [{
            targets = [ "127.0.0.1:${toString config.services.cadvisor.port}" ];
          }];
          scrape_interval = "15s";
        }
        {
          job_name = "pihole";
          static_configs = [{
            targets = [ "127.0.0.1:9006" ];
          }];
          scrape_interval = "15s";
        }
        {
          job_name = "fritzbox";
          static_configs = [{
            targets = [ "127.0.0.1:9007" ];
          }];
          scrape_interval = "15s";
        }];
    };
    loki = {
      enable = true;
      configFile = ./loki.yaml;
    };
    postgresql = {
      enable = true;
      package = pkgs.postgresql_14;
      enableTCPIP = true;
      checkConfig = true;
      extraPlugins = with pkgs.postgresql_14.pkgs; [ postgis ];
      authentication = ''
        local   all             all                                     trust
        host    all             all             192.168.178.0/24        scram-sha-256
        host    replication     all             192.168.178.0/24       scram-sha-256
      '';
    };
    redis = {
      servers = {
        pi4 = {
          bind = "0.0.0.0";
          enable = true;
          openFirewall = true;
          port = 6379;
        };
      };
    };
  };

  systemd.services = {
    promtail = {
      description = "Promtail service for Loki";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        ExecStart = ''
          ${pkgs.grafana-loki}/bin/promtail --config.file ${./promtail.yaml}
        '';
      };
    };

    prometheus-node-exporter = {
      serviceConfig = {
        RestrictAddressFamilies = [ "AF_NETLINK" ];
      };
    };

    prometheus-pihole-exporter = {
      serviceConfig.ExecStart = lib.mkForce ''
        ${pkgs.bash}/bin/bash -c "${pkgs.prometheus-pihole-exporter}/bin/pihole-exporter \
          -pihole_api_token 365d84d2a47d3c42c8a261973d5c53e652113e9f88b4bcfb07439c62da333559 \
          -pihole_hostname 127.0.0.1 \
          -pihole_port 8000 \
          -pihole_protocol http \
          -port 9006"
      '';
    };
  };

  users = {
    users.sebastian = {
      isNormalUser = true;
      extraGroups = [ "wheel" "docker" ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDdFv1ZT9EzT2mrapiucBoe83vJDwRuBri245aYL+dmI sebastian@monad"
      ];
      shell = pkgs.zsh;
    };
  };

  # Enable GPU acceleration
  hardware.raspberry-pi."4".fkms-3d.enable = true;

  hardware.pulseaudio.enable = false;

  virtualisation = {
    containers = {
      enable = true;
      containersConf = {
        settings = {
          engine.helper_binaries_dir = [ "${pkgs.netavark}/bin" ];
        };
      };
    };
    podman = {
      enable = true;
      dockerCompat = true;
    };

    oci-containers.backend = "podman";

    oci-containers.containers = {
      fritz = {
        autoStart = true;
        cmd = [ "--config" "/app/fritz-exporter.yml" ];
        extraOptions = [ "--pull=newer" ];
        image = "pdreker/fritz_exporter:2.2.1";
        ports = [ "9007:9007/tcp" ];
        volumes = [
          "/home/sebastian/fritz-exporter.yml:/app/fritz-exporter.yml"
        ];
      };

      pihole = {
        autoStart = true;
        environmentFiles = [ /home/sebastian/.pihole.env ];
        extraOptions = [
          "--cap-add=NET_ADMIN"
          "--network=host"
          "--pull=newer"
        ];
        image = "pihole/pihole:2022.12.1";
        volumes = [
          "/home/sebastian/etc-pihole:/etc/pihole"
          "/home/sebastian/etc-dnsmasq.d:/etc/dnsmasq.d"
        ];
      };
    };
  };
}
