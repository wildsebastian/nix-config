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
  };

  outputs =
    { self
    , darwin
    , emacs
    , home-manager
    , nixos-hardware
    , nixpkgs
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

      formatter = {
        aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.nixpkgs-fmt;
        aarch64-linux = nixpkgs.legacyPackages.aarch64-linux.nixpkgs-fmt;
      };
    };
}
