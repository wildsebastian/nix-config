#!/bin/sh

cd ~/src/nixpkgs
git pull origin master
cd ~/src/nix-darwin
git pull upstream master
git push origin master
cd ~
darwin-rebuild switch