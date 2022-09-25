{
  description = "My nix(OS) config";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/8e819696fea32a10916a6bcf70d4798b7bcf56c1";
    };

    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs = {
      url = "github:nix-community/emacs-overlay";
    };
  };

  outputs = { self, darwin, emacs, nixpkgs }@attrs: {
    darwinConfigurations = {
      "monad" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = attrs;
        modules = [ ./monad/config.nix ];
      };
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."monad".pkgs;
  };
}
