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
        (evil-collection-init))

    (use-package evil-magit
      :after evil)

    (use-package treemacs-evil
      :after evil)

    (use-package magit)
    
    (use-package direnv)

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

    (use-package lsp-mode
      :commands lsp)
    (use-package lsp-ui :commands lsp-ui-mode)
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
    (use-package dap-mode)

    (use-package nix-mode
      :mode "\\.nix\\'")
    
    (use-package elpy
      :defer t
      :init
      (advice-add 'python-mode :before 'elpy-enable))
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
    dap-mode
    dashboard
    direnv
    elpy
    evil
    evil-collection
    evil-magit
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
