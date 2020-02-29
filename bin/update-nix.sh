#!/bin/sh

cd ~/src/nixpkgs
git pull origin master
cd ~/src/nix-darwin
git pull upstream master
git push origin master
cd ~
darwin-rebuild switch

nix-collect-garbage --delete-older-than 7d

/Users/sebastian/.nixpkgs/bin/allow-mosh.sh
