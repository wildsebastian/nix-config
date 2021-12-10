(require 'package)

(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")
    ("elpa" . "https://elpa.gnu.org/packages/")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;(package-initialize 'noactivate)
(eval-when-compile
  (require 'use-package))

; Set nix path for emacs deamon started by launchd
(setenv "NIX_PATH"
  (concat
    "darwin-config="
    (getenv "HOME")
    "/.nixpkgs/configuration.nix"
    ":"
    "nixos-config=/etc/nixos/configuration.nix" ":"
    "nixpkgs="
    (getenv "HOME")
    "/.nix-defexpr/nixpkgs" ":"
    "darwin="
    (getenv "HOME")
    "/.nix-defexpr/darwin"
  )
)

(setenv "SHELL" "/run/current-system/sw/bin/zsh")

(setq
  backup-directory-alist '(("." . "~/.emacs_backups"))
  backup-by-copying t
  version-control t
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2)

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
(display-battery-mode t)
(global-hl-line-mode t)
(global-display-line-numbers-mode 1)
(setq column-number-mode t)
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

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

;; Modes that are always active
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-nord t))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq evil-want-C-i-jump nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-undo-system 'undo-tree)
  (evil-mode)
  ;; set leader key in all states
  (evil-set-leader nil (kbd "C-SPC"))
  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC")))

(use-package evil-collection
  :ensure t
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
  :ensure t
  :custom-face
  (mode-line ((t (:height 1.0))))
  (mode-line-inactive ((t (:height 1.0))))
  :custom
  (doom-modeline-mode t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-env-version t)
  (doom-modeline-env-load-string "?")
  (doom-modeline-project-detection 'project)
  (doom-modeline-height 30)
  (doom-modeline-bar-width 1)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t))

(use-package consult
  :ensure t

  :bind (:map evil-normal-state-map
          ("<leader>si" . consult-ripgrep))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package all-the-icons
  :ensure t)

(use-package perspective
  :ensure t
  :bind (:map evil-normal-state-map
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
  :ensure t
  :after all-the-icons
  :custom
  (centaur-tabs-style "wave")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker "o")
  (centaur-tabs-close-button "×")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-mode t)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  (:map evil-normal-state-map
    ("t n" . centaur-tabs-forward)
    ("t p" . centaur-tabs-backward)
    ("t s" . centaur-tabs-counsel-switch-group)
    ("t g" . centaur-tabs-group-by-projectile-project)))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

(use-package rg
  :ensure t
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
  (evil-define-key 'normal 'global (kbd "<leader>swp") 'rg-web))

(use-package flyspell
  :hook
  (python-mode . flyspell-prog-mode)
  (haskell-mode . flyspell-prog-mode)
  :config
  (setq-default ispell-program-name "aspell")
  (flyspell-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "Welcome Sebastian")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((projects . 5)
                          (recents . 5)
                          (agenda . 5))))

(use-package tramp
  :ensure t)

(use-package treemacs
  :ensure t
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
  :ensure t
  :after (treemacs evil))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  :bind
  (:map evil-normal-state-map
    ("<leader>pff" . projectile--find-file)
    ("<leader>pfw" . projectile-find-file-other-window)
    ("<leader>pt" . projectile-test-project)
    ("<leader>pr" . projectile-run-project)))

(use-package format-all
  :ensure t
  :hook
  (python-mode . format-all-mode))

(use-package magit
  :ensure t
  :bind
  (:map evil-normal-state-map
    ("<leader>gm" . magit)
    ;; fetch
    ("<leader>gfa" . magit-fetch-all))
    ;; rebase
    ("<leader>grs" . magit-rebase)
    ("<leader>gri" . magit-rebase-interactive)
    ("<leader>gra" . magit-rebase-abort)
    ("<leader>grc" . magit-rebase-continue)
    ;; checkout
    ("<leader>gbc" . magit-branch-checkout)
    ;; stash
    ("<leader>gss" . magit-stash)
    ("<leader>gsp" . magit-stash-pop))

(use-package magit-delta
  :ensure t
  :hook
  (magit-mode . magit-delta-mode)
  :custom
  (magit-delta-delta-args
    '("--24-bit-color" "always"
      "--features" "magit-delta"
      "--color-only")))

(use-package forge
  :ensure t
  :after magit)

(use-package transient
  :ensure t
  :after magit)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode))

(use-package git-gutter-fringe
  :ensure t)

(defun ws/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package sql)

(use-package ob-sql)

(use-package org
  :hook (org-mode . ws/org-mode-setup)
  :bind
  (:map evil-normal-state-map
    ("<leader>oj" . org-roam-dailies-capture-today))
  :config
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((ditaa . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . t)
      (python . t)
      (restclient . t)
      (shell . t)
      (sql . t)
      (ocaml . t)))
  (setq org-ellipsis " ▾"
    org-hide-emphasis-markers t
    org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 2
    org-hide-block-startup t
    org-src-preserve-indentation nil
    org-startup-folded t
    org-cycle-separator-lines 2
    org-default-notes-file "~/notes/inbox.org"
    org-agenda-files (list "~/notes/inbox.org")
    org-modules (quote (org-habit))
    org-treat-insert-todo-heading-as-state-change t
    org-log-done 'note
    org-log-into-drawer t
    org-habit-show-all-today t
    org-todo-keywords
      '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)"))
    org-todo-keyword-faces
      '(("TODO" . org-warning) ("WIP" . "yellow") ("DONE" . "green"))
    org-archive-location "~/notes/archive.org::* From %s"
    org-latex-listings 'minted
    org-latex-packages-alist '(("" "minted"))
    org-latex-pdf-process
      '("pdflatex --shell-escape -synctex=1 -interaction=nonstopmode -file-line-error -output-directory %o %f"
        "pdflatex --shell-escape -synctex=1 -interaction=nonstopmode -file-line-error -output-directory %o %f")
    )

  (use-package org-superstar
    :ensure t
    :after org
    :hook (org-mode . org-superstar-mode)
    :config (org-superstar-configure-like-org-bullets))

  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))
    (set-face-attribute 'org-block nil :background "#000000" :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-drawer nil :background "#000000")
    (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-column nil :background nil)
    (set-face-attribute 'org-column-title nil :background nil)

  (use-package evil-org
    :ensure t
    :after org
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (use-package org-pandoc-import
    :after org
    :ensure nil
    :quelpa (org-pandoc-import
              :fetcher github
              :repo "tecosaur/org-pandoc-import"
              :files ("*.el" "filters" "preprocessors"))))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :hook
  (after-init . org-roam-setup)
  :custom
  (org-roam-directory (file-truename "~/ideas/"))
  (org-roam-dailies-directory "~/journal/")
  (org-roam-dailies-capture-templates
    '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  :bind (:map org-roam-mode-map
          (("C-c n l" . org-roam)
           ("C-c n f" . org-roam-find-file)
           ("C-c n g" . org-roam-graph))
          :map org-mode-map
          (("C-c n i" . org-roam-insert))
          (("C-c n I" . org-roam-insert-immediate))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; Modes that are loaded under certain circumstances
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(set-face-foreground 'fill-column-indicator "red")
(add-hook 'prog-mode-hook #'company-mode)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package python-mode
  :mode ("\\.py" . python-mode)
  :custom
  (python-shell-interpreter "python3"))

(use-package haskell-mode
  :ensure t
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
  :ensure t
  :mode
  ("\\.purs" . purescript-mode))

(use-package psc-ide
  :ensure t
  :init
  (add-hook 'purescript-mode-hook (
    lambda ()
      (psc-ide-mode)
      (company-mode)
      (flycheck-mode)
      (turn-on-purescript-indentation))))

(use-package agda2-mode
  :ensure t
  :mode
  ("\\.agda\\'" . agda2-mode))

(use-package yaml-mode
  :ensure t
  :mode
  ("\\.yml\\'" . yaml-mode))

(use-package idris-mode
  :ensure t
  :mode
  ("\\.idr" . idris-mode))

(use-package php-mode
  :ensure t
  :mode
  ("\\.php" . php-mode))

(use-package web-mode
  :ensure t
  :mode
  ("\\.tpl\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package scss-mode
  :ensure t
  :mode
  ("\\.scss\\'" . scss-mode))

(use-package typescript-mode
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package proof-general
  :ensure t
  :mode ("\\.v\\'" . coq-mode)
  :custom
  (proof-layout-windows 'hybrid)
  (proof-splash-enable nil)
  )

(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$"
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'" . terraform-mode))

(use-package rustic
  :ensure t
  :custom
  (rustic-format-on-save t))

(use-package lsp-mode
  :ensure t
  :hook
  ((python-mode haskell-mode scala-mode purescript-mode) . lsp-deferred)
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
  (lsp-modeline-code-actions-enable nil)
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :after
  (lsp-mode flycheck)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 25))

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-formatting-provider "ormolu"))

(use-package lsp-pyright
  :ensure t)

(use-package lsp-metals
  :ensure t
  :config (setq lsp-metals-treeview-show-when-views-received t))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t
  :commands dap-debug
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package dap-python
  :custom
  (dap-python-debugger 'debugpy))

(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (lsp-completion-provider :capf)
  (global-company-mode t))

;; Synchronize company with pcomplete for org mode
;; https://marcohassan.github.io/bits-of-experience/pages/emacs/
(defun trigger-org-company-complete ()
  "Begins company-complete in org-mode buffer after pressing #+ chars."
  (interactive)
  (if (string-equal "#" (string (preceding-char)))
    (progn
      (insert "+")
      (company-complete))
    (insert "+")))

(eval-after-load 'org '(define-key org-mode-map
	       (kbd "+") 'trigger-org-company-complete))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package company-coq
  :ensure t
  :hook (coq-mode . company-coq-mode))

(use-package company-posframe
  :ensure t
  :config
  (company-posframe-mode 1))

(use-package flycheck
  :ensure t
  :hook
  (python-mode . flycheck-mode)
  (haskell-mode . flycheck-mode))

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup)

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package docker
  :ensure t
  :bind
  (:map evil-normal-state-map
    ("<leader>d" . docker)))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package restclient
  :ensure t)

(use-package ob-restclient
  :ensure t)

(use-package ein
  :ensure t)

(use-package vterm
  :ensure t)

(use-package vterm-toggle
  :ensure t
  :bind (("<leader>vt" . vterm-toggle)
         ("<leader>vn" . vterm-toggle-forward)
         ("<leader>vp" . vterm-toggle-backward)))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
