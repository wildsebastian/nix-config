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

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default gc-cons-threshold 100000000)
(setq-default read-process-output-max (* 1024 1024)) ;; 1mb
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font 12"))
(set-face-attribute 'default t :font "Iosevka Nerd Font 12")
(prefer-coding-system 'utf-8)
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

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

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
  :config
  (setq evil-want-integration nil
    evil-collection-company-use-tng nil)
  (evil-collection-init))

;; http://sodaware.sdf.org/notes/emacs-daemon-doom-modeline-icons/
(defun enable-doom-modeline-icons (_frame)
  (setq doom-modeline-icon t))

(add-hook 'after-make-frame-functions #'enable-doom-modeline-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-unicode-fallback t)
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t))

(use-package counsel)
(use-package swiper)
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (evil-define-key 'normal 'global (kbd "<leader>x") 'counsel-M-x)
  (evil-define-key 'normal 'global (kbd "<leader>si") 'counsel-rg)
  (ivy-mode)
)

(use-package perspective
  :after counsel
  :custom
  (persp-initial-frame-name "Main")
  :config
  (evil-define-key 'normal 'global (kbd "<leader>wsb") 'persp-counsel-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>ws") 'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader>wn") 'persp-next)
  (evil-define-key 'normal 'global (kbd "<leader>wkb") 'persp-kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>wkw") 'persp-kill)
  (evil-define-key 'normal 'global (kbd "<leader>wb") 'persp-ibuffer)
  (unless persp-mode
    (persp-mode 1)))

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (evil-define-key 'normal 'global (kbd "<leader>cs") 'centaur-tabs-counsel-switch-group)
  (evil-define-key 'normal 'global (kbd "<leader>cg") 'centaur-tabs-group-by-projectile-project)
  :bind
  (:map evil-normal-state-map
    ("g t" . centaur-tabs-forward)
    ("g T" . centaur-tabs-backward)))

(use-package fzf
  :config
  (evil-define-key 'normal 'global (kbd "<leader>sf") 'fzf))

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

  (evil-define-key 'normal 'global (kbd "<leader>sc") 'rg)
  (evil-define-key 'normal 'global (kbd "<leader>shp") 'rg-haskell)
  (evil-define-key 'normal 'global (kbd "<leader>spp") 'rg-python)
  (evil-define-key 'normal 'global (kbd "<leader>swp") 'rg-web)
)

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
  (setq dashboard-items '((projects . 5)
                          (registers . 5))))

(use-package tramp)

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (evil-define-key 'normal 'global (kbd "<leader>to") 'treemacs)
  (evil-define-key 'normal 'global (kbd "<leader>ts") 'treemacs-switch-workspace)
  (evil-define-key 'normal 'global (kbd "<leader>tp") 'treemacs-projectile)
  (treemacs-load-theme "Default"))

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
  :config
  (evil-define-key 'normal 'global (kbd "<leader>pt") 'projectile-test-project)
  (evil-define-key 'normal 'global (kbd "<leader>pr") 'projectile-run-project)
  )

(use-package format-all
  :hook
  (python-mode . format-all-mode))

(use-package magit
  :config
  (evil-define-key 'normal 'global (kbd "<leader>m") 'magit))

(use-package forge
  :after magit)

(use-package transient
  :after magit)

(use-package yasnippet-snippets)

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package auto-yasnippet)

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

;; Modes that are loaded under certain circumstances
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(set-face-foreground 'fill-column-indicator "red")

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
  (lsp-diagnostic-package :flycheck)
  (lsp-prefer-capf t)
  (read-process-output-max (* 1024 1024))
  :config
  (setq lsp-log-io nil)
  (setq lsp-enable-folding nil)
  (setq lsp-diagnostic-package :none)
  (setq lsp-completion-provider :capf)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-links nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-idle-delay 0.500))

(use-package lsp-haskell
  :config
  (setq lsp-haskell-formatting-provider "fourmolu"))

(use-package lsp-pyright)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-peek nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-delay 3)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t))

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
  :config
  (evil-define-key 'normal 'global (kbd "<leader>d") 'docker))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "/run/current-system/sw/bin/zsh")
  (setq vterm-max-scrollback 10000)
  (evil-define-key 'normal 'global (kbd "<leader>v") 'vterm-other-window))
