# My Nix(OS) configuration

On a new macOS system run the following to first install nix (the package manager) and nix-darwin.

[Nix](https://nixos.org/download.html#nix-install-macos): 
```sh
sh <(curl -L https://nixos.org/nix/install)
```

[Nix Darwin](https://github.com/lnl7/nix-darwin):
```sh
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
./result/bin/darwin-installer
```

This creates an example config in `~/.nixpkgs/darwin-configuration` and bootstraps the system.
Afterwards we can clone this repository and build the system using nix flakes. You can omit the
path to flake if you are in the directory already.
```sh
darwin-rebuild switch --flake <path_to_flake>/.#
```

You can append the host of the machine in the flake if nix can not match it automatically.
```sh
darwin-rebuild switch --flake <path_to_flake>/.#<hostname>
```

## Note

After emacs setup, run `M-x all-the-icons-install-fonts`.
