#!/bin/sh

cd ~/src/nixpkgs
git pull origin master
cd ~/src/nix-darwin
git pull origin master
cd ~
darwin-rebuild switch

nix-collect-garbage --delete-old
