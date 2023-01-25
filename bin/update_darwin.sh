#!/bin/sh

cd ~/.nixpkgs
nix flake update
darwin-rebuild switch --flake .#
sudo nix-collect-garbage --delete-older-than 7d
cd ~/
