#!/bin/sh

cd ~/src/nix-config
nix flake update
darwin-rebuild switch --flake .#
sudo nix-collect-garbage --delete-older-than 7d
cd ~
