{
  description = "My nix(OS) config";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/8e819696fea32a10916a6bcf70d4798b7bcf56c1";
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware/master";
    };

    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs = {
      url = "github:nix-community/emacs-overlay";
    };
  };

  outputs = { self, darwin, emacs, nixpkgs, nixos-hardware }@attrs: {
    # Macbook Air
    darwinConfigurations = {
      "monad" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = attrs;
        modules = [ ./monad/config.nix ];
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
  };
}
