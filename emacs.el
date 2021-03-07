(require 'package)
(package-initialize 'noactivate)
(eval-when-compile
  (require 'use-package))

; Set nix path for emacs deamon started by launchd
(setenv "NIX_PATH"
  (concat
    "darwin-config="
    (getenv "HOME")
    "/.nixpkgs/darwin-configuration.nix"
    ":"
    "nixpkgs="
    (getenv "HOME")
    "/.nix-defexpr/nixpkgs" ":"
    "darwin="
    (getenv "HOME")
    "/.nix-defexpr/darwin"
  )
)

(setq comp-deferred-compilation t)
(setq comp-async-report-warnings-errors nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default gc-cons-threshold 100000000)
(setq-default read-process-output-max (* 1024 1024)) ;; 1mb
(add-to-list 'default-frame-alist '(font . "Iosevka Extended 14"))
(set-face-attribute 'default t :font "Iosevka Extended 14")
(set-default-coding-systems 'utf-8)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(flymake-mode -1)
(tab-bar-mode -1)
(global-hl-line-mode t)
(toggle-frame-fullscreen)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(save-place-mode t)
(setq ring-bell-function 'ignore blink-cursor-mode nil)
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

;; Modes that are always active
(use-package base16-theme
  :config
  (load-theme 'base16-classic-dark t))

(set-face-foreground 'font-lock-comment-face "#b8b8b8")
(set-face-foreground 'font-lock-comment-delimiter-face "#b8b8b8")

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  ;; set leader key in all states
  (evil-set-leader nil (kbd "C-SPC"))
  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC"))
  (setq evil-undo-system 'undo-tree))

(use-package evil-collection
  :after evil
  :custom
  (evil-want-integration nil)
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(setq evil-emacs-state-cursor   '(box "#7cafc2"))
(setq evil-insert-state-cursor  '(bar "#7cafc2"))
(setq evil-motion-state-cursor  '(box "#ba8baf"))
(setq evil-normal-state-cursor  '(box "#a1b56c"))
(setq evil-replace-state-cursor '(bar "#ab4642"))
(setq evil-visual-state-cursor  '(box "#dc9656"))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom-face
  (mode-line ((t (:height 1.0))))
  (mode-line-inactive ((t (:height 1.0))))
  :custom
  (doom-modeline-unicode-fallback t)
  (doom-modeline-height 30)
  (doom-modeline-bar-width 1)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t))

(use-package counsel)
(use-package swiper)
(use-package ivy
  :bind (:map evil-normal-state-map
    ("<leader>x" . counsel-M-x)
    ("<leader>si" . counsel-rg))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode)
)

(use-package perspective
  :after counsel
  :bind (:map evil-normal-state-map
    ("<leader>wsb" . persp-counsel-switch-buffer)
    ("<leader>ws" . persp-switch)
    ("<leader>wn" . persp-next)
    ("<leader>wkb" . persp-kill-buffer)
    ("<leader>wkw" . persp-kill)
    ("<leader>wb" . persp-ibuffer))
  :custom
  (persp-initial-frame-name "Main")
  :config
  (unless persp-mode
    (persp-mode 1)))

(use-package centaur-tabs
  :demand
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t)
  (centaur-tabs-set-bar 'under x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  :bind
  (:map evil-normal-state-map
    ("t n" . centaur-tabs-forward)
    ("t p" . centaur-tabs-backward)
    ("t s" . centaur-tabs-counsel-switch-group)
    ("t g" . centaur-tabs-group-by-projectile-project)))

(use-package fzf
  :bind
  (:map evil-normal-state-map
    ("<leader>sf" . fzf)))

(use-package rg
  :config

  (rg-define-search rg-haskell
    "Search through all haskell files in a project"
    :dir project
    :format literal
    :flags '("--vimgrep")
    :files "*.{hs,lhs}"
    :menu ("Custom" "h" "haskell"))

  (rg-define-search rg-python
    "Search through all python files in a project"
    :dir project
    :format literal
    :flags '("--vimgrep")
    :files "*.{py}"
    :menu ("Custom" "p" "python"))

  (rg-define-search rg-web
    "Search through all web files in a project"
    :dir project
    :format literal
    :flags '("--vimgrep")
    :files "*.{html,js,ts,scss,css}"
    :menu ("Custom" "w" "web"))

  :bind
  (:map evil-normal-state-map
    ("<leader>sc" . rg)
    ("<leader>shp" . rg-haskell)
    ("<leader>spp" . rg-python)
    ("<leader>swp" .rg-web)))

(use-package flyspell
  :hook
  (python-mode . flyspell-prog-mode)
  (haskell-mode . flyspell-prog-mode)
  :config
  (setq-default ispell-program-name "aspell")
  (flyspell-mode 1))

(use-package all-the-icons)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "Welcome Sebastian")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((projects . 5)
                          (recents . 5)
                          (agenda . 5)
                          (bookmarks . 5)
                          (registers . 5))))

(use-package tramp)

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-load-theme "Default")
  :bind
  (:map evil-normal-state-map
    ("<leader>to" . treemacs)
    ("<leader>ts" . treemacs-switch-workspace)
    ("<leader>tp" . treemacs-projectile)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package projectile
  :config
  (projectile-mode)
  :custom
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  :bind
  (:map evil-normal-state-map
    ("<leader>pt" . projectile-test-project)
    ("<leader>pr" . projectile-run-project)))

(use-package format-all
  :hook
  (python-mode . format-all-mode))

(use-package magit
  :bind
  (:map evil-normal-state-map
    ("<leader>gs" . magit)
    ("<leader>gfa" . magit-fetch-all)))

(use-package forge
  :after magit)

(use-package transient
  :after magit)

(use-package git-gutter
  :config
  (global-git-gutter-mode))

(use-package git-gutter-fringe)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '( "~/.yasnippets" )))

(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
    (call-interactively #'company-complete-common))))

(add-hook 'company-mode-hook
  (lambda () (substitute-key-definition
    'company-complete-common
    'company-yasnippet-or-completion
    company-active-map)))

(use-package org)

;; Modes that are loaded under certain circumstances
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(set-face-foreground 'fill-column-indicator "red")
(add-hook 'prog-mode-hook #'company-mode)

(use-package direnv
  :init
  (add-hook 'prog-mode-hook #'direnv-update-environment)
  :config
  (direnv-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package python-mode
  :mode ("\\.py" . python-mode)
  :custom
  (python-shell-interpreter "python3"))

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
  (setq haskell-hoogle-url "http://127.0.0.1:8181/?hoogle=%s")
  )

(use-package purescript-mode
  :mode
  ("\\.purs" . purescript-mode))

(use-package psc-ide
  :init
  (add-hook 'purescript-mode-hook (
    lambda ()
      (psc-ide-mode)
      (company-mode)
      (flycheck-mode)
      (turn-on-purescript-indentation))))

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

(use-package typescript-mode
  :mode
  ("\\.ts\\'" . typescript-mode))

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

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode))

(use-package lsp-mode
  :hook
  ((python-mode haskell-mode) . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  (before-save . lsp-format-buffer)
  :custom
  (lsp-prefer-capf t)
  (read-process-output-max (* 1024 1024))
  (lsp-modeline-diagnostics-enable nil)
  (lsp-log-io nil)
  (lsp-enable-folding nil)
  (lsp-diagnostic-package :flycheck)
  (lsp-completion-provider :capf)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-lens-enable nil)
  (lsp-enable-links nil)
  (lsp-restart 'auto-restart)
  (lsp-enable-file-watchers nil)
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil))

(use-package lsp-haskell
  :config
  (setq lsp-haskell-formatting-provider "fourmolu"))

(use-package lsp-pyright)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode)
(use-package dap-python)

(use-package which-key
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

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

(use-package docker
  :bind
  (:map evil-normal-state-map
    ("<leader>d" . docker)))

(use-package restclient)

(use-package vterm
  :commands vterm
  :custom
  (vterm-shell "/run/current-system/sw/bin/zsh")
  (vterm-max-scrollback 10000)
  :bind
  (:map evil-normal-state-map
    ("<leader>v" . vterm-other-window)))

(use-package elfeed
  :custom
  (elfeed-feeds
    '(
       ; Mathematics
       ("https://api.quantamagazine.org/feed/" mathematics physics computer-science)

       ; News
       ; zeit.de
       ("http://newsfeed.zeit.de/politik/index" zeit politics)
       ("http://newsfeed.zeit.de/wirtschaft/index" zeit economics)
       ("http://newsfeed.zeit.de/wissen/index" zeit science)
       ("http://newsfeed.zeit.de/digital/index" zeit digital)
       ("http://newsfeed.zeit.de/arbeit/index" zeit work)
       ("http://newsfeed.zeit.de/zeit-magazin/index" zeit magazine)
       ))
  :bind
  (:map evil-normal-state-map
    ("<leader>fo" . elfeed)))
