{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  myEmacsConfig = pkgs.writeText "default.el" ''
    (require 'package)
    (package-initialize 'noactivate)
    (eval-when-compile
      (require 'use-package))

    (add-to-list 'default-frame-alist '(font . "JetBrains Mono-13"))
    (set-face-attribute 'default t :font "JetBrains Mono-13")
    (prefer-coding-system 'utf-8)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (flymake-mode -1)
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type 'relative)

    ;; Modes that are always active
    (use-package nord-theme
      :config
      (load-theme 'nord t))

    (use-package editorconfig
      :config
      (editorconfig-mode 1))

    (use-package evil
      :init
      (setq evil-want-keybinding nil)
      :config
      (evil-mode))

    (use-package evil-collection
      :after evil
      :config
      (setq evil-want-integration nil
       evil-collection-company-use-tng nil)
      (evil-collection-init))

    (use-package fzf)

    (use-package helm-config
      :config
      (setq-default helm-M-x-fuzzy-match t))

    (use-package helm-ag
      :after helm)

    (use-package helm-projectile
      :after helm)

    (use-package all-the-icons)

    (use-package dashboard
      :config
      (dashboard-setup-startup-hook)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
      (setq dashboard-banner-logo-title "Welcome Sebastian")
      (setq dashboard-startup-banner 'logo)
      (setq dashboard-items '((projects . 5)
                              (registers . 5))))

    (use-package treemacs
      :init
      (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

    (use-package treemacs-evil
      :after treemacs)

    (use-package projectile
      :config
      (projectile-mode +1))

    (use-package fill-column-indicator
      :hook
      (haskell-mode . fci-mode)
      (nix-mode . fci-mode)
      (python-mode . fci-mode)
      :config
      (setq fci-rule-width 1)
      (setq fci-rule-color "red"))

    (use-package magit)

    (use-package forge
      :after magit)

    (use-package transient
      :after magit)

    (use-package evil-magit
      :after (evil magit))

    ;; Modes that are loaded under certain circumstances
    (use-package direnv
      :init
      (add-hook 'before-hack-local-variables-hook #'direnv-update-environment)
      :config
      (direnv-mode))

    (use-package nix-mode
      :mode "\\.nix\\'")

    (use-package python
      :mode ("\\.py" . python-mode))

    (defun haskell-mode-after-save-handler () nil)

    (use-package haskell-mode
      :mode
      ("\\.hs" . haskell-mode)
      ("\\.ghci$" . ghci-script-mode)
      ("\\.cabal$" . haskell-cabal-mode)
      :interpreter
      (("runghc" . haskell-mode)
       ("runhaskell" . haskell-mode))
      :config
      (setq haskell-stylish-on-save t))

    (use-package lsp-mode
      :hook
      (python-mode . lsp-deferred)
      :commands
      (lsp lsp-deferred)
      :config
      (setq lsp-prefer-flymake nil)
      (setq lsp-enable-snippet nil)
      (setq lsp-pyls-plugins-pylint-enabled nil)
      (setq lsp-pyls-configuration-sources ["flake8"])
      (setq lsp-pyls-plugins-pycodestyle-max-line-length 120))

    (use-package lsp-ui
      :after lsp-mode
      :commands lsp-ui-mode)

    (use-package lsp-treemacs
      :after lsp-mode
      :commands lsp-treemacs-errors-list)

    (use-package company
      :config
      (setq company-idle-delay 0)
      (setq company-minimum-prefix-length 2))

    (use-package company-box
      :hook (company-mode . company-box-mode))

    (use-package company-lsp
      :config
      (push 'company-lsp company-backends))

    (use-package flycheck
      :hook
      (python-mode . flycheck-mode)
      (haskell-mode . flycheck-mode))

    (use-package flycheck-haskell
      :commands flycheck-haskell-setup)
  '';
in
emacsWithPackages (epkgs: (
  with epkgs.melpaPackages; with epkgs.elpaPackages; [
    (pkgs.runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
      ''
    )
    all-the-icons
    company
    company-box
    company-lsp
    dashboard
    direnv
    editorconfig
    evil
    evil-collection
    evil-magit
    fill-column-indicator
    flycheck
    flycheck-haskell
    forge
    fzf
    haskell-mode
    helm
    helm-ag
    helm-projectile
    lsp-mode
    lsp-ui
    lsp-treemacs
    magit
    nix-mode
    nord-theme
    page-break-lines
    projectile
    transient
    treemacs
    treemacs-evil
    use-package
  ]
))
