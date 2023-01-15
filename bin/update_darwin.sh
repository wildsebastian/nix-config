#!/bin/sh

cd ~/.nixpkgs

git stash

nix flake update

darwin-rebuild switch --flake .#

git add -u

git commit -m "Update flake inputs"

git push

git stash pop
