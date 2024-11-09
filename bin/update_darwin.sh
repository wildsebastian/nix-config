#!/usr/bin/env bash

cd ~/src/nix-config || exit
nix flake update
darwin-rebuild switch --flake .#
sudo nix-collect-garbage --delete-older-than 7d
nix-collect-garbage --delete-older-than 7d
cd ~ || exit
