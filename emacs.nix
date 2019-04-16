{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacsMacport;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  myEmacsConfig = pkgs.writeText "default.el" ''
    (require 'package)
    (package-initialize 'noactivate)
    (eval-when-compile
      (require 'use-package))

    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (global-linum-mode 1)

    (use-package base16-theme
      :config
      (load-theme 'base16-default-dark t))

    (use-package exec-path-from-shell
      :config
      (exec-path-from-shell-copy-env "PATH")
      (exec-path-from-shell-copy-env "NIX_PATH"))

    (use-package fill-column-indicator
      :config
      (setq fci-rule-column 79)
      (setq fci-rule-width 1)
      (setq fci-rule-color "red")
      (add-hook 'haskell-mode-hook 'fci-mode)
      (add-hook 'nix-mode-hook 'fci-mode)
      (add-hook 'python-mode-hook 'fci-mode))

    (use-package fzf)

    (use-package evil
      :ensure t
      :init
      ;; This is optional since it's already set to t by default.
      (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      :config
      (evil-mode 1))

    (use-package evil-collection
      :after (company evil)
      :config
      (require 'company-tng)
      (evil-collection-init))

    (use-package evil-magit
      :after evil)

    (use-package undo-tree
      :config
      (global-undo-tree-mode))

    (use-package goto-chg)

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

    (use-package company-lsp
      :commands company-lsp)

    (use-package proof-site
      :mode
      ("\\.v\\'" . coq-mode))

    (use-package haskell-mode
      :after direnv
      :mode
      ("\\.hs\\'" . haskell-mode)
      :config
      (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
      (add-hook 'haskell-mode-hook 'company-mode)
      (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
      (add-hook 'haskell-interactive-mode-hook 'company-mode)
      (add-to-list 'completion-ignored-extensions ".hi")
      (custom-set-variables '(haskell-stylish-on-save t)))

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
emacsWithPackages (epkgs: (
  with epkgs.melpaPackages; with epkgs.elpaPackages; [
    (pkgs.runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
      ''
    )
    base16-theme
    company
    company-lsp
    company-nixos-options
    direnv
    editorconfig
    elpy
    evil
    evil-collection
    evil-magit
    exec-path-from-shell
    fill-column-indicator
    flycheck
    flycheck-haskell
    fzf
    goto-chg
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
    undo-tree
    use-package
    web-mode
  ]
))
