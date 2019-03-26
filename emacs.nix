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

    (use-package exec-path-from-shell
      :config
      (exec-path-from-shell-copy-env "PATH")
      (exec-path-from-shell-copy-env "NIX_PATH"))

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
      (add-hook 'haskell-mode-hook 'flycheck-mode)
      :config (global-flycheck-mode))

    (use-package lsp-mode
      :commands lsp
      :config
      (add-hook 'python-mode-hook #'lsp)
      (add-hook 'haskell-mode-hook #'lsp))

    (use-package lsp-ui
      :commands lsp-ui-mode)

    (use-package lsp-haskell
      :config
      (setq lsp-haskell-process-path-hie "hie-wrapper"))

    (use-package company-lsp
      :commands company-lsp)

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
      :after direnv
      :mode
      ("\\.hs\\'" . haskell-mode)
      :config
      (add-hook 'haskell-mode-hook 'company-mode)
      (add-hook 'haskell-interactive-mode-hook 'company-mode))

    (setq flymake-no-changes-timeout nil)
    (setq flymake-start-syntax-check-on-newline nil)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (auto-save-visited-mode 1)
    (setq auto-save-visited-interval 1)

    (use-package web-mode
      :defer t
      :mode
      ("\\.css\\'" . web-mode)
      ("\\.html\\'" . web-mode)
      ("\\.htm\\'" . web-mode)
      ("\\.js\\'" . web-mode))

    (use-package nix-mode
      :mode ("\\.nix\\'" . nix-mode))

    (use-package python-mode
      :mode ("\\.py\\'" . python-mode))

    (use-package elpy
      :config
      (add-hook 'python-mode-hook 'elpy-mode))
  '';
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    (pkgs.runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
      ''
    )
    company
    company-lsp
    company-nixos-options
    direnv
    editorconfig
    elpy
    evil
    evil-collection
    exec-path-from-shell
    fill-column-indicator
    flycheck
    flycheck-haskell
    haskell-mode
    ivy
    lsp-haskell
    lsp-mode
    lsp-ui
    magit
    nix-mode
    projectile
    projectile-direnv
    proof-general
    solarized-theme
    use-package
    web-mode
  ]))
