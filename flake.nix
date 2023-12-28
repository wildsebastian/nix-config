{
  description = "My nix(OS) config";

  inputs = {
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs = {
      url = "github:nix-community/emacs-overlay";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware/master";
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs";
    };

    phps = {
      url = "github:fossar/nix-phps";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , darwin
    , emacs
    , home-manager
    , nixos-hardware
    , nixpkgs
    , phps
    , pre-commit-hooks
    }@attrs: {
      # Macbook Air
      darwinConfigurations = {
        "monad" = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = attrs;
          modules = [
            ./monad/config.nix
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.sebastian = import ./monad/home.nix;
              home-manager.extraSpecialArgs = { inherit phps; };
            }
          ];
        };
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."monad".pkgs;

      # RaspberryPi4
      nixosConfigurations.pi4-0 = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = attrs;
        modules = [
          nixos-hardware.nixosModules.raspberry-pi-4
          ./pi4/config.nix
        ];
      };


      checks = {
        aarch64-darwin = {
          pre-commit-check = pre-commit-hooks.lib.aarch64-darwin.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              shellcheck.enable = true;
              terraform-format.enable = true;
            };
          };
        };

        aarch64-linux = {
          pre-commit-check = pre-commit-hooks.lib.aarch64-linux.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              shellcheck.enable = true;
              terraform-format.enable = true;
            };
          };
        };
      };

      formatter = {
        aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.nixpkgs-fmt;
        aarch64-linux = nixpkgs.legacyPackages.aarch64-linux.nixpkgs-fmt;
      };
    };
}
