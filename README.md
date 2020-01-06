# nix-config

My local/server nix config for macOS

## Fresh single-user install on Catalina

1. Create /etc/synthetic.conf with the two entries
  ```
  nix
  run
  ```
2. Reboot and then execute
  (Taken from: https://github.com/NixOS/nix/issues/2925#issuecomment-539570232)
  ```
  sudo diskutil apfs addVolume disk1 APFSX Nix -mountpoint /nix
  sudo diskutil enableOwnership /nix
  sudo chflags hidden /nix  # Don't show the Nix volume on the desktop
  echo "LABEL=Nix /nix apfs rw" | sudo tee -a /etc/fstab

  sudo diskutil apfs addVolume disk1 APFSX NixDarwin -mountpoint /run
  sudo diskutil enableOwnership /run
  sudo chflags hidden /run  # Don't show the Nix volume on the desktop
  echo "LABEL=NixDarwin /run apfs rw" | sudo tee -a /etc/fstab
  ```
3. Install nix (single user)
  ```
  curl https://nixos.org/nix/install | sh
  ```
4. Clone config
  ```
  git clone git@github.com:wildsebastian/nix-config.git .nixpkgs
  ```
5. Clone package repository if you don't want to use the default channel
  ```
  git clone git@github.com:wildsebastian/nixpkgs.git
  ```
6. Remove default channels and symlink the package repository
  ```
  cd .nix-defexpr
  rm -rf *
  ln -s $HOME/nixpkgs $HOME/.nix-defexpr/nixpkgs
  ```
7. Install nix-darwin
  ```
  git clone git@github.com:wildsebastian/nix-darwin.git
  ln -s $HOME/nix-darwin $HOME/.nix-defexpr/darwin
  export NIX_PATH=darwin=$HOME/.nix-defexpr/darwin:darwin-config=$HOME/.nixpkgs/darwin-configuration.nix:nixpkgs=$HOME/.nix-defexpr/nixpkgs
  $(nix-build '<darwin>' -A system --no-out-link)/sw/bin/darwin-rebuild switch
  ```



 * After macOS updates:
    - Backup /etc/bashrc, /etc/zshrc, /etc/zprofile
    - Remove /etc/bashrc, /etc/zshrc, /etc/zprofile
    - darwin-rebuild switch
