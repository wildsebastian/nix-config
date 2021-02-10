#!/bin/sh

cd ~/.nixpkgs/overlays/emacs-overlay
git pull upstream master
git push origin master
cd ~/.nixpkgs
