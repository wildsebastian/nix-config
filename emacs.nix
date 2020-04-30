{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  myEmacsConfig = pkgs.writeText "default.el" ''
    (require 'package)
    (package-initialize 'noactivate)
    (eval-when-compile
      (require 'use-package))

    (add-to-list 'default-frame-alist '(font . "Hack Nerd Font-12"))
    (set-face-attribute 'default t :font "Hack Nerd Font-12")
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
      :defer 1
      :config
      (editorconfig-mode 1))

    (use-package evil
      :defer 1
      :init
      (setq evil-want-keybinding nil)
      :config
      (evil-mode))

    (use-package evil-collection
      :defer 2
      :after evil
      :config
      (setq evil-want-integration nil
       evil-collection-company-use-tng nil)
      (evil-collection-init))

    (use-package fzf
      :defer 5)

    (use-package vterm
      :defer 2)

    (use-package helm-config
      :defer 5
      :config
      (setq-default helm-M-x-fuzzy-match t)
      (global-set-key "\C-x\C-m" 'helm-M-x)
      (global-set-key "\C-c\C-m" 'helm-M-x)
      (define-key evil-normal-state-map (kbd ",") 'helm-M-x))

    (use-package helm-ag
      :defer 5
      :after helm)

    (use-package helm-projectile
      :defer 5
      :after helm)

    (use-package all-the-icons
      :defer 8)

    (use-package dashboard
      :config
      (dashboard-setup-startup-hook)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
      (setq dashboard-banner-logo-title "Welcome Sebastian")
      (setq dashboard-startup-banner 'logo)
      (setq dashboard-items '((projects . 5)
                              (registers . 5))))

    (use-package tramp
      :defer 8)

    (use-package treemacs
      :defer 2
      :init
      (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
      :config
      (treemacs-load-theme "Default"))

    (use-package treemacs-evil
      :defer 2
      :after treemacs)

    (use-package projectile
      :defer 2
      :config
      (projectile-mode +1))

    (use-package fill-column-indicator
      :hook
      (haskell-mode . fci-mode)
      (nix-mode . fci-mode)
      (php-mode . fci-mode)
      (python-mode . fci-mode)
      (web-mode . fci-mode)
      :config
      (setq fci-rule-width 1)
      (setq fci-rule-color "red"))

    (use-package format-all
      :hook
      (python-mode . format-all-mode))

    (use-package magit
      :defer 12)

    (use-package forge
      :after magit)

    (use-package transient
      :after magit)

    (use-package evil-magit
      :after (evil magit))

    (use-package docker
      :defer 5
      :bind ("C-c d" . docker))

    ;; Modes that are loaded under certain circumstances
    (use-package direnv
      :init
      (add-hook 'prog-mode-hook #'direnv-update-environment)
      :config
      (direnv-mode))

    (use-package nix-mode
      :mode "\\.nix\\'")

    (use-package python
      :mode ("\\.py" . python-mode))

    (use-package haskell-mode
      :mode
      ("\\.hs" . haskell-mode)
      ("\\.ghci$" . ghci-script-mode)
      ("\\.cabal$" . haskell-cabal-mode)
      :interpreter
      (("runghc" . haskell-mode)
       ("runhaskell" . haskell-mode))
      :config
      (require 'haskell-doc)
      (setq haskell-mode-stylish-haskell-path "stylish-haskell")
      (setq haskell-stylish-on-save t))

    (use-package dante
      :after haskell-mode
      :commands 'dante-mode
      :init
      (add-hook 'haskell-mode-hook 'flycheck-mode)
      (add-hook 'haskell-mode-hook 'dante-mode)
      :config
      (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

    (add-hook 'dante-mode-hook
     '(lambda () (flycheck-add-next-checker 'haskell-dante
                                       '(warning . haskell-hlint))))

    (use-package yaml-mode
      :mode
      ("\\.yml\\'" . yaml-mode))

    (use-package idris-mode
      :mode
      ("\\.idr" . idris-mode))

    (use-package php-mode
      :mode
      ("\\.php" . php-mode))

    (use-package web-mode
      :mode
      ("\\.tpl\\'" . web-mode)
      :config
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-css-indent-offset 2))

    (use-package scss-mode
      :mode
      ("\\.scss\\'" . scss-mode))

    (use-package markdown-mode
      :commands (markdown-mode gfm-mode)
      :mode
      (("README\\.md\\'" . gfm-mode)
       ("\\.md\\'" . markdown-mode)
       ("\\.markdown\\'" . markdown-mode))
      :init
      (setq markdown-command "multimarkdown"))

    (use-package proof-general
      :mode ("\\.v\\'" . coq-mode)
      :config
      (setq proof-layout-windows 'hybrid))

    (use-package lsp-mode
      :hook
      (python-mode . lsp-deferred)
      :commands
      (lsp lsp-deferred)
      :config
      (setq lsp-auto-configure t)
      (setq lsp-prefer-flymake nil)
      (setq lsp-enable-snippet nil)
      (setq lsp-pyls-plugins-pylint-enabled nil)
      (setq lsp-pyls-configuration-sources ["flake8"])
      (setq lsp-pyls-plugins-pycodestyle-max-line-length 120))

    (use-package lsp-ui
      :after lsp-mode
      :commands lsp-ui-mode
      :hook
      (python-mode . flycheck-mode)
      :config
      (setq lsp-ui-doc-enable t
            lsp-ui-sideline-enable t
            lsp-ui-flycheck-enable t))

    (use-package lsp-treemacs
      :after lsp-mode
      :commands lsp-treemacs-errors-list)

    (use-package dap-mode
      :after lsp-mode
      :config
      (require 'dap-python)
      (require 'dap-ui)
      (dap-mode t)
      (dap-ui-mode t)
      (tooltip-mode t))

    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (use-package company
      :config
      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
      (setq company-idle-delay 0)
      (setq company-minimum-prefix-length 1))

    (use-package company-box
      :hook (company-mode . company-box-mode))

    (use-package company-lsp
      :after (lsp-mode company)
      :config
      (push 'company-lsp company-backends)
      (setq company-lsp-cache-candidates 'auto)
      (setq company-lsp-async t)
      (setq company-lsp-enable-recompletion t))

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
    dante
    dap-mode
    dashboard
    direnv
    docker
    editorconfig
    emacs-libvterm
    evil
    evil-collection
    evil-magit
    fill-column-indicator
    flycheck
    flycheck-haskell
    forge
    format-all
    fzf
    haskell-mode
    helm
    helm-ag
    helm-projectile
    idris-mode
    lsp-mode
    lsp-ui
    lsp-treemacs
    magit
    markdown-mode
    nix-mode
    nord-theme
    org
    page-break-lines
    php-mode
    projectile
    proof-general
    scss-mode
    transient
    tramp
    treemacs
    treemacs-evil
    treemacs-magit
    treemacs-projectile
    use-package
    web-mode
    yaml-mode
  ]
))
