{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacsUnstable;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  myEmacsConfig = pkgs.writeText "default.el" ''
    (require 'package)
    (package-initialize 'noactivate)
    (eval-when-compile
      (require 'use-package))

    (setq gc-cons-threshold 100000000)
    (setq read-process-output-max (* 1024 1024)) ;; 1mb

    (add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
    (set-face-attribute 'default t :font "JetBrains Mono-14")
    (prefer-coding-system 'utf-8)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (flymake-mode -1)
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type 'relative)

    ;; Modes that are always active
    (use-package zenburn-theme
      :config
      (load-theme 'zenburn t))

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

    (use-package flyspell
      :hook
      (python-mode . flyspell-prog-mode)
      (haskell-mode . flyspell-prog-mode)
      :config
      (setq-default ispell-program-name "aspell")
      (flyspell-mode 1))

    (use-package helm-config
      :config
      (setq-default helm-M-x-fuzzy-match t)
      (global-set-key "\C-x\C-m" 'helm-M-x)
      (global-set-key "\C-c\C-m" 'helm-M-x)
      (define-key evil-normal-state-map (kbd ",") 'helm-M-x))

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

    (use-package tramp)

    (use-package treemacs
      :init
      (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
      :config
      (treemacs-load-theme "Default"))

    (use-package treemacs-evil
      :after treemacs)

    (use-package projectile
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
      (setq haskell-stylish-on-save t)
      ;; hoogle setup
      (setq haskell-hoogle-port-number 8181)
      (setq haskell-hoogle-server-command (lambda (port) (list
                                            "hoogle"
                                            "server"
                                            "-p" (number-to-string port)
                                            "--host"
                                            "127.0.0.1"
                                            "--local"
                                            "--haskell"
                                            "-n")))
      (setq haskell-hoogle-url "http://127.0.0.1/?hoogle=%s")
      )

    (use-package dante
      :after haskell-mode
      :commands 'dante-mode
      :init
      (add-hook 'haskell-mode-hook 'flycheck-mode)
      (add-hook 'haskell-mode-hook 'dante-mode)
      :config
      (setq-default dante-repl-command-line
        '("cabal" "new-repl" dante-target "--builddir=dist-newstyle/dante"))
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
      (lsp-mode . lsp-enable-which-key-integration)
      :config
      (setq lsp-idle-delay 0.500))

    (use-package lsp-ui :commands lsp-ui-mode)
    (use-package helm-lsp :commands helm-lsp-workspace-symbol)
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
    (use-package dap-mode)
    (use-package dap-python)

    (use-package which-key
      :config
      (setq which-key-show-early-on-C-h t)
      (setq which-key-idle-delay 10000)
      (setq which-key-idle-secondary-delay 0.05)
      (setq which-key-popup-type 'minibuffer)
      (define-key evil-normal-state-map (kbd "c") 'which-key-C-h-dispatch)
      (which-key-mode))

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
      (setq company-idle-delay 0.0)
      (setq company-minimum-prefix-length 1))

    (use-package company-box
      :hook (company-mode . company-box-mode))

    (use-package company-coq
      :hook (coq-mode . company-coq-mode))

    (use-package flycheck
      :hook
      (python-mode . flycheck-mode)
      (haskell-mode . flycheck-mode))

    (use-package flycheck-haskell
      :commands flycheck-haskell-setup)

    (use-package vterm)
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
    company-coq
    company-org-roam
    dante
    dap-mode
    dashboard
    deft
    direnv
    editorconfig
    vterm
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
    helm-lsp
    helm-projectile
    idris-mode
    lsp-mode
    lsp-ui
    lsp-treemacs
    magit
    markdown-mode
    nix-mode
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
    which-key
    web-mode
    yaml-mode
    zenburn-theme
  ]
))
