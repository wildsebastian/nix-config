{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  myEmacsConfig = pkgs.writeText "default.el" ''
    (require 'package)
    (package-initialize 'noactivate)
    (eval-when-compile
      (require 'use-package))

    (when (version<= "26.0.50" emacs-version )
      (global-display-line-numbers-mode))

    (setq inhibit-startup-screen t)
    (setq initial-scratch-message nil)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode -1)

    (use-package solarized-theme
      :init
      (setq solarized-theme t)
      :config
      (load-theme 'solarized-dark t))

    (use-package evil
      :init
      (setq evil-want-keybinding nil)
      :config
      (evil-mode 1))

    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))

    (use-package flycheck
      :defer 2
      :init
      (add-hook 'elpy-mode-hook 'flycheck-mode)
      :config (global-flycheck-mode))

    (use-package projectile
      :commands projectile-mode
      :bind-keymap ("C-c p" . projectile-command-map)
      :defer t
      :config
      (projectile-global-mode))

    (use-package direnv
      :config
      (direnv-mode 1))

    (use-package editorconfig
      :config
      (editorconfig-mode 1))

    (use-package magit)

    (use-package company
      :init
      (add-hook 'after-init-hook 'global-company-mode)
      (add-hook 'haskell-interactive-mode-hook 'company-mode)
      :config
      (add-to-list 'company-backends 'company-ghci)
      (add-to-list 'company-backends 'company-nixos-options))

    (use-package proof-site
      :mode
      ("\\.v\\'" . coq-mode))

    (use-package haskell-mode
      :mode
      ("\\.hs\\'" . haskell-mode)
      :config
      (add-hook 'haskell-mode-hook 'company-mode))

    (use-package web-mode
      :defer t
      ("\\.css\\'" . web-mode)
      ("\\.html\\'" . web-mode)
      ("\\.htm\\'" . web-mode)
      ("\\.js\\'" . web-mode))

    (use-package nix-mode
      :mode ("\\.nix\\'" . nix-mode))

    (use-package elpy
      :mode ("\\.py\\'" . elpy))
  '';
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    (pkgs.runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
      ''
    )
    company
    company-ghci
    company-nixos-options
    direnv
    editorconfig
    elpy
    evil
    evil-collection
    fill-column-indicator
    flycheck
    flycheck-haskell
    haskell-mode
    ivy
    magit
    nix-mode
    projectile
    projectile-direnv
    proof-general
    solarized-theme
    use-package
    web-mode
  ]))
