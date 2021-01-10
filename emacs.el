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
    "/.nix-defexpr/nixpkgs"
    ":"
    "darwin="
    (getenv "HOME")
    "/.nix-defexpr/darwin"
  )
)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
(set-face-attribute 'default t :font "JetBrains Mono-14")
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
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\( . ?\))
        (?\< . ?\>)
        (?\{ . ?\})))
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
  (evil-mode)
  ;; set leader key in all states
  (evil-set-leader nil (kbd "C-SPC"))
  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC")))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration nil
    evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package counsel)
(use-package swiper)
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (evil-define-key 'normal 'global (kbd "<leader>x") 'counsel-M-x)
  (ivy-mode)
)

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
  (purescript-mode . fci-mode)
  (python-mode . fci-mode)
  (web-mode . fci-mode)
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "red"))

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
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  (haskell-mode . lsp-deferred)
  :config
  (setq lsp-enable-file-watchers nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil))

(use-package lsp-haskell
  :config
  (setq lsp-haskell-formatting-provider "fourmolu"))

(use-package lsp-pyright)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-diagnostic-max-lines 10)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-alignment 'window)
  (setq lsp-ui-doc-use-webkit t))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)
(use-package dap-python)

(use-package which-key
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-setup-side-window-bottom)
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
