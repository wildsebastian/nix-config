# nix-config

My local nix config

## Fresh multi-user install on Catalina

 * https://github.com/NixOS/nix/issues/2925#issuecomment-539570232
 * After macOS installs or updates:
    - Backup /etc/bashrc, /etc/zshrc, /etc/zprofile
    - Remove /etc/bashrc, /etc/zshrc, /etc/zprofile
    - darwin-rebuild switch
