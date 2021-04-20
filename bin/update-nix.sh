#!/bin/sh

cd ~/.nixpkgs
git stash
git pull origin master
cd ~/.nixpkgs/overlays/emacs-overlay
git pull origin master
cd ~/.nixpkgs
git stash pop
cd ~/src/nixpkgs
git pull origin master
cd ~/src/nix-darwin
git pull origin master
cd ~
darwin-rebuild switch

sudo nix-collect-garbage --delete-older-than 7d
