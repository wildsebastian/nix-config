{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  myEmacsConfig = pkgs.writeText "default.el" ''
    (require 'package)
    (package-initialize 'noactivate)
    (eval-when-compile
      (require 'use-package))

    (use-package dashboard
      :ensure t
      :config
      (setq dashboard-startup-banner 'logo)
      (dashboard-setup-startup-hook))

    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (use-package zenburn-theme
      :config
      (load-theme 'zenburn t))

    (use-package fill-column-indicator
      :config
      (setq fci-rule-column 80
            fci-rule-color "red"
            fci-rule-width 1))

    (setq tab-width 2
      indent-tabs-mode nil)

    (setq make-backup-files nil)
    (defalias 'yes-or-no-p 'y-or-n-p)

    (setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
    (show-paren-mode t)

    (use-package all-the-icons)

    (use-package evil
      :init
      (setq evil-want-keybinding nil)
      :config
      (evil-mode 1))

    (use-package evil-collection
      :after evil
      :config
        (setq evil-want-integration nil
         evil-collection-company-use-tng nil)
        (evil-collection-init))

    (use-package evil-magit
      :after evil)

    (use-package treemacs-evil
      :after evil)

    (use-package magit)

    (use-package direnv
      :config
      (direnv-mode))

    (use-package projectile
      :config
      (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
      (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
      (projectile-mode +1))

    (use-package treemacs
      :defer t
      :init
      (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

    (use-package treemacs-projectile
      :after treemacs projectile
      :ensure t)

    (use-package spaceline-config
      :config
      (spaceline-spacemacs-theme))

    (use-package editorconfig
      :config
      (editorconfig-mode 1))

    (use-package company
      :config
      (setq company-idle-delay 0.3)

      (global-company-mode 1)

      (global-set-key (kbd "C-<tab>") 'company-complete))

    (use-package company-lsp
      :requires company
      :config
        (push 'company-lsp company-backends)
        ;; Disable client-side cache because the LSP server does a better job.
        (setq company-transformers nil
              company-lsp-async t
              company-lsp-cache-candidates nil))

    (use-package lsp-mode
      :after direnv
      :hook (python-mode . lsp)
      :commands lsp
      :config
        (setq lsp-enable-snippet nil))
    (use-package lsp-ui :commands lsp-ui-mode)
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
    (use-package dap-mode)

    (use-package nix-mode
      :mode "\\.nix\\'")

    (use-package elpy
      :after
        direnv
        python-mode
      :init
      (elpy-enable))
  '';
in
emacsWithPackages (epkgs: (
  with epkgs.melpaPackages; [
    (pkgs.runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
      ''
    )
    all-the-icons
    company
    company-lsp
    dap-mode
    dashboard
    direnv
    editorconfig
    elpy
    evil
    evil-collection
    evil-magit
    fill-column-indicator
    lsp-mode
    lsp-treemacs
    lsp-ui
    magit
    nix-mode
    page-break-lines
    projectile
    spaceline
    spaceline-all-the-icons
    treemacs
    treemacs-evil
    treemacs-projectile
    use-package
    zenburn-theme
  ]
))
