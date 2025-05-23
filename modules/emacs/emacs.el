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

(use-package emacs
  :init
  (setenv "PATH" (concat
                  "/Users/sebastian/.nix-profile/bin:"
                  "/etc/profiles/per-user/sebastian/bin:"
                  "/run/current-system/sw/bin:"
                  "/nix/var/nix/profiles/default/bin:"
                  (getenv "PATH")
                  ))
  (setenv "SHELL" "/etc/profiles/per-user/sebastian/bin/zsh")

  (setq
   backup-directory-alist '(("." . "~/.emacs_backups"))
   backup-by-copying t
   version-control t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2)

  (setq comp-deferred-compilation t)
  (setq pixel-scroll-precision-mode t)
  (setq comp-async-report-warnings-errors nil)
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default gc-cons-threshold 100000000)
  (setq-default read-process-output-max (* 1024 1024)) ;; 1mb
  (set-default-coding-systems 'utf-8)
  (setq x-underline-at-descent-line t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tab-bar-mode -1)
  (display-battery-mode -1)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (setq column-number-mode t)
  (setq display-line-numbers-type 'relative)
  (fset 'yes-or-no-p 'y-or-n-p)
  (show-paren-mode t)
  (setq-default fill-column 80)
  (setq-default
   auto-save-interval 60)

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

  ;; scratch buffer settings
  (setq initial-scratch-message ""
        initial-major-mode 'org-mode)

  (setq evil-want-keybinding nil)

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
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; Auto reload buffer when the file on disk changes
  (global-auto-revert-mode t)
  (setq auto-revert-use-notify nil))

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Modes that are always active
(use-package doom-themes
  :ensure t)

(use-package kaolin-themes
  :ensure t
  ;; :custom-face
  ;; (font-lock-comment-delimiter-face ((t (:weight bold :foreground "#fce085"))))
  ;; (font-lock-comment-face ((t (:weight bold :foreground "#fce085"))))
  ;; :config
  ;; (load-theme 'kaolin-dark t)
  :custom
  (kaolin-themes-italic-comments t)
  (kaolin-themes-distinct-fringe t)
  (kaolin-themes-git-gutter-solid t))

(use-package modus-themes
  :ensure t
  :init
  (setq
   modus-themes-fringes 'intense
   modus-themes-mixed-fonts t
   modus-themes-headings
   '((1 . (variable-pitch bold 1.3))
     (2 . (variable-pitch semibold 1.2))
     (3 . (variable-pitch 1.1))
     (4 . (1.05))
     (5 . (1.05))
     (6 . (1.05))
     (7 . (1.05))
     (8 . (1.05)))
   modus-themes-scale-headings t
   modus-themes-org-blocks 'tinted-background)
  (load-theme 'modus-vivendi))

(use-package haki-theme
  :ensure t)

(set-face-attribute 'default nil :font "Fira Code" :width 'regular :height 120 :weight 'normal)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :width 'regular :height 120 :weight 'normal)
(set-face-attribute 'variable-pitch nil :font "Fira Code" :width 'regular :height 120 :weight 'normal)

(defun my-modus-themes-custom-faces ()
  (modus-themes-with-colors (custom-set-faces
                             `(org-document-title ((,class :inherit (bold variable-pitch) :height 1.8))))))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq evil-want-C-i-jump nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (setq evil-undo-system 'undo-tree)
  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'debugger-mode 'motion)

  ;; cursor settings
  (setq evil-emacs-state-cursor    '("#649bce" box))
  (setq evil-normal-state-cursor   '("#ebcb8b" box))
  (setq evil-operator-state-cursor '("#ebcb8b" hollow))
  (setq evil-visual-state-cursor   '("#677691" box))
  (setq evil-insert-state-cursor   '("#eb998b" (bar . 2)))
  (setq evil-replace-state-cursor  '("#eb998b" hbar))
  (setq evil-motion-state-cursor   '("#ad8beb" box))
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init)
  :custom
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-y-for-yank t))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-anzu
  :ensure t
  :after evil
  :config
  (global-anzu-mode t))

(use-package undo-tree
  :ensure t
  :after evil
  :bind
  (:map evil-normal-state-map
        ("C-r" . undo-tree-redo)
        ("u"   . undo-tree-undo))
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode 1))

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :custom-face
  (mode-line ((t (:height 1.0))))
  (mode-line-inactive ((t (:height 1.0))))
  :custom
  (projectile-dynamic-mode-line t)
  (doom-modeline-hud nil)
  (doom-modeline-bar-width 1)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode nil)
  (doom-modeline-buffer-file-name-style 'auto)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-window-width-limit 85)
  (doom-modeline-battery nil)
  (doom-modeline-env-version t)
  (doom-modeline-env-load-string "?")
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-indent-info t)
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-lsp t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t))

(use-package which-key
  :ensure t
  :custom-face
  (which-key-separator-face ((t (:inherit nil :background nil :foreground "#545c5e"))))
  :config
  (setq which-key-popup-type 'minibuffer
        which-key-idle-delay 0.3)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package general
  :ensure t
  :config
  (defun ws/switch-agda-input ()
    (interactive)
    (activate-input-method "Agda"))
  (general-define-key
   "M-e" '(tempel-expand :which-key "Tempel snippet expand")
   )
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :prefix "SPC"

   ;; top level functions
   "/" '(affe-grep :which-key "grep")
   ":" '(projectile-find-file :which-key "p-find file")
   "[" '(org-capture :which-key "org capture")
   "." '(org-roam-capture :which-key "roam capture")
   "," '(org-roam-dailies-capture-today :which-key "journal")
   "'" '(vterm-toggle :which-key "vterm")
   "=" '(ace-window :which-key "ace-window")
   "x" '(execute-extended-command :which-key "M-x")
   "q" '(save-buffers-kill-terminal :which-key "quit emacs")
   "#" '(comment-dwim :which-key "add/remove comment")

   ;; applications
   "a" '(nil :which-key "applications")
   "ad" '(docker :which-key "docker")
   "aw" '(writeroom-mode :which-key "writeroom")

   ;; Buffers
   "b" '(nil :which-key "buffer")
   "bb" '(consult-buffer :which-key "switch buffers")
   "bk" '(kill-current-buffer :which-key "kill current buffer")

   ;; cape
   "c" '(nil :which-key "cape")
   "cp" '(nil :which-key "Completion at point variants")
   "cpf" '(cape-file :which-key "File")
   "cph" '(cape-history :which-key "History")
   "cpu" '(cape-rfc1345 :which-key "Unicode")

   ;; magit
   "g" '(nil :which-key "magit")
   "gm" '(magit :which-key "status")
   "gci" '(blamer-show-commit-info :which-key "blame commit info")
   "gfa" '(magit-fetch-all :which-key "fetch all")
   "grs" '(magit-rebase :which-key "rebase")
   "gri" '(magit-rebase-interactive :which-key "rebase interactive")
   "gra" '(magit-rebase-abort :which-key "rebase abort")
   "grc" '(magit-rebase-continue :which-key "rebase continue")
   "gbc" '(magit-branch-checkout :which-key "branch + checkout")
   "gss" '(magit-stash :which-key "stash")
   "gsp" '(magit-stash-pop :which-key "stash pop")

   "i" '(nil :which-key "input")
   "ia" '(ws/switch-agda-input :which-key "Agda input method")

   ;; lsp
   "lfd" '(lsp-find-definition :which-key "find definition")
   "lfr" '(lsp-find-implementation :which-key "find references")
   "ln" '(flymake-goto-next-error :which-key "next error")
   "lp" '(flymake-goto-previous-error :which-key "previous error")
   "la" '(lsp-code-actions-at-point :which-key "code action")
   "lr" '(lsp-rename :which-key "rename")

   ;; org-roam
   "o" '(nil :which-key "org")
   "oc" '(org-capture :which-key "org capture")
   "ojd" '(org-roam-dailies-capture-today :which-key "journal today")
   "ojt" '(org-roam-dailies-capture-tomorrow :which-key "journal tomorrow")
   "ojy" '(org-roam-dailies-capture-yesterday :which-key "journal yesterday")
   "or" '(nil :which-key "org roam")
   "orc" '(org-roam-capture :which-key "org roam capture")
   "ori" '(org-roam-node-insert :which-key "org roam insert")
   "orf" '(org-roam-node-find :which-key "org roam find")

   ;; projectile
   "p" '(nil :which-key "projectile")
   "pff" '(projectile-find-file :which-key "find file")
   "psb" '(projectile-switch-to-buffer :which-key "switch buffer")
   "psp" '(projectile-switch-project :which-key "switch project")
   "pkb" '(projectile-kill-buffers :which-key "kill project buffers")
   "pr" '(nil :which-key "project run")
   "prt" '(projectile-test-project :which-key "run tests")
   "prp" '(projectile-run-project :which-key "run project")
   "prs" '(projectile-run-shell-command-in-root :which-key "run shell command")

   ;; snippets
   "s" '(nil :which-key "tempel snippets")
   "si" '(tempel-insert :which-key "tempel insert")

   ;; terminal
   "t" '(nil :which-key "terminal")
   "tv" '(multi-vterm :which-key "vterm")
   "tr" '(multi-vterm-rename-buffer :which-key "rename vterm buffer")
   "ts" '(shell-command :which-key "shell command")

   ;; perspective workspace
   "w" '(nil :which-key "workspace")
   "ws" '(persp-switch :which-key "switch workspace")
   "wn" '(persp-next :which-key "next workspace")
   "wkb" '(persp-kill-buffer* :which-key "kill buffer in workspace")
   "wkw" '(persp-kill :which-key "kill workspace")
   "wb" '(persp-ibuffer :which-key "switch buffer in workspace")
   "wr" '(persp-rename :which-key "rename workspace")
   ))

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Apple Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode)))

(use-package tempel
  :ensure t
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (use-package tempel-collection
    :ensure t))

(use-package consult
  :ensure t

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

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "~/src")
  (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package dwim-shell-command
  :ensure t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

(require 'dwim-shell-commands)

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
  (("C-q" . embark-act)         ;; pick some comfortable binding
   ("M-;" . embark-dwim)        ;; good alternative: M-.
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

;; Add extensions
(use-package cape
  :ensure t)

(use-package affe
  :ensure t
  :after (consult orderless)
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :config
  (nerd-icons-completion-mode))

(use-package dirvish
  :ensure t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))

(use-package perspective
  :ensure t
  :custom
  (persp-initial-frame-name "Main")
  (persp-suppress-no-prefix-key-warning t)
  :config
  (unless persp-mode
    (persp-mode 1)))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode 1))

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
    :menu ("Custom" "w" "web")))

(use-package flyspell
  :hook
  (python-mode . flyspell-prog-mode)
  (haskell-mode . flyspell-prog-mode)
  :config
  (setq-default ispell-program-name "aspell")
  (flyspell-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src/")))
  :config
  (setq projectile-sort-order 'recently-active))

(use-package page-break-lines
  :ensure t)

(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-projects-backend 'projectile)
  (setq initial-buffer-choice (lambda ()
                                (get-buffer-create "*dashboard*")
                                ;; Needed to show icons on startup
                                (dashboard-refresh-buffer)))
  (setq dashboard-startup-banner "~/logo256.png")
  (setq dashboard-set-footer t)
  (setq dashboard-center-content t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info nil)
  (setq dashboard-items '((projects . 5) (recents  . 5) (bookmarks . 5))))

(use-package xterm-color
  :ensure t
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (setq compilation-scroll-output 1)
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

(use-package proced
  :commands proced
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-auto-update-interval 1
        proced-goal-attribute nil
        proced-enable-color-flag t)
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm)))
  (setq-default proced-format 'custom))

(use-package polymode
  :ensure t)

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'php-mode apheleia-mode-alist)
		    '(prettier))
  (apheleia-global-mode t))

(use-package magit
  :ensure t
  :config
  (setq magit-git-executable "/etc/profiles/per-user/sebastian/bin/git"))

(use-package magit-delta
  :ensure t
  :hook
  (magit-mode . magit-delta-mode)
  :custom
  (magit-delta-delta-args
   '("--24-bit-color" "always"
     "--no-gitconfig"
     "--color-only")))

(use-package magit-todos
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package transient
  :ensure t
  :after magit)

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package blamer
  :ensure t
  :defer 20
  :custom
  (blamer-idle-time 1.0)
  (blamer-min-offset 70)
  (blamer-author-formatter "✎ %s ")
  (blamer-datetime-formatter "[%s] ")
  (blamer-commit-formatter "● %s")
  (blamer-prettify-time-p t)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t))))

(use-package flymake
  :custom
  (help-at-pt-timer-delay 0.2)
  (help-at-pt-display-when-idle '(flymake-overlay)))

(use-package sql
  :defer 10)

(use-package ob-sql
  :defer 10)

(use-package restclient
  :defer t
  :ensure t)

(use-package ob-restclient
  :defer t
  :ensure t)

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . t)
     (ocaml . t)
     (python . t)
     (restclient . t)
     (shell . t)
     (sql . t)))
  :custom
  (org-ellipsis "")
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 2)
  (org-hide-block-startup t)
  (org-src-preserve-indentation nil)
  (org-startup-folded t)
  (org-startup-with-inline-images t)
  (org-image-actual-width 360)
  (org-cycle-separator-lines 2)
  (org-modules (quote (org-habit)))
  (org-treat-insert-todo-heading-as-state-change t)
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-habit-show-habits-only-for-today t)
	(org-todo-keywords '(
                       (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	                     (sequence "BACKLOG(b)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|"
                                 "DELEGATED(D)" "CANCELLED(c)")))
  (org-archive-location "~/appunti/archive.org::* From %s")
  (org-agenda-files '("~/appunti/gtd.org" "~/appunti/someday.org"))
  (org-agenda-include-diary t)
  (org-latex-listings 'minted)
  (org-latex-packages-alist '(("" "minted")))
  (org-latex-toc-command "\\tableofcontents \\clearpage")
  (org-latex-pdf-process
   '("pdflatex --shell-escape -synctex=1 -interaction=nonstopmode -file-line-error -output-directory %o %f"
     "pdflatex --shell-escape -synctex=1 -interaction=nonstopmode -file-line-error -output-directory %o %f"
     "pdflatex --shell-escape -synctex=1 -interaction=nonstopmode -file-line-error -output-directory %o %f"))
  (org-capture-templates
   '(("i" "Inbox" entry (file "~/appunti/inbox.org"))
	   ("t" "Todo" entry (file+headline "~/appunti/gtd.org" "Tasks")
	    "* TODO %?\n  %i\n  %a")
	   ("s" "Someday" entry (file "~/appunti/someday.org")
	    "* TODO %?\n  %i\n  %a")))

  (use-package org-contrib
    :ensure t)

  ;; (use-package org-modern
  ;;   :ensure t
  ;;   :commands (org-modern-mode org-modern-agenda)
  ;;   :custom
  ;;   (org-modern-table-vertical 1)
  ;;   (org-modern-table-horizontal 1)
  ;;   (org-modern-block t)
  ;;   (org-modern-block-fringe t)
  ;;   (set-face-attribute 'org-modern-symbol nil :family "Iosevka Nerd Font Mono" :height 130)
  ;;   :init
  ;;   (setq org-modern-todo t
  ;;         org-modern-variable-pitch nil)
  ;;   (global-org-modern-mode))

  (use-package evil-org
    :ensure t
    :after org
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(custom-set-faces
 '(org-document-title ((t (:height 180 :weight medium)))))

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t)
  ;; (org-roam-db-autosync-mode)
  :hook
  (after-init . org-roam-setup)
  :custom
  (org-roam-directory (file-truename "~/appunti/thoughts/"))
  (org-roam-dailies-directory "~/appunti/journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; (use-package citar
;;   :ensure t
;;   :bind
;;   (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
;;   :custom
;;   (citar-bibliography '("~/bib/references.bib"))
;;   (org-cite-global-bibliography '("~/bib/references.bib"))
;;   (org-cite-insert-processor 'citar)
;;   (org-cite-follow-processor 'citar)
;;   (org-cite-activate-processor 'citar)
;;   (citar-bibliography org-cite-global-bibliography))
;;
;; (use-package citar-embark
;;   :after citar embark
;;   :no-require
;;   :config (citar-embark-mode))

(use-package org-auto-tangle
  :ensure t
  :after org
  :hook (org-mode . org-auto-tangle-mode))

;; Modes that are loaded under certain circumstances
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'company-mode)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package haskell-mode
  :ensure t
  :defer t
  :after tree-sitter
  :mode
  ("\\.hs" . haskell-mode)
  ("\\.ghci$" . ghci-script-mode)
  ("\\.cabal$" . haskell-cabal-mode)
  :interpreter
  (("runghc" . haskell-mode)
   ("runhaskell" . haskell-mode))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(haskell-mode . haskell))
  (require 'haskell-doc)
  ;; hoogle setup
  (setq haskell-hoogle-command "hoogle")
  (setq haskell-hoogle-port-number 8100)
  (setq haskell-hoogle-server-command (lambda (port) (list
                                                      "hoogle"
                                                      "server"
                                                      "-p" (number-to-string port)
                                                      "--host"
                                                      "127.0.0.1"
                                                      "--local"
                                                      "--haskell"
                                                      "-n")))
  (setq haskell-hoogle-url "http://127.0.0.1:8100/?hoogle=%s"))

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

(use-package idris2-mode
  :defer t
  :mode
  ("\\.idr" . idris2-mode))

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package proof-general
  :ensure t
  :defer t
  :mode ("\\.v\\'" . coq-mode)
  :custom
  (proof-layout-windows 'hybrid)
  (proof-three-window-mode-policy 'hybrid)
  (proof-shrink-windows-tofit t)
  (proof-splash-enable t))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode))

(use-package lsp-mode
  :ensure t
  :defer t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((csharp-mode
    haskell-mode
    nix-mode
    purescript-mode
    ) . lsp-deferred)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-disabled-clients '(tfls))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :ensure t
  :after lsp-mode)

(use-package company
  :ensure t
  :defer t
  :after lsp-mode
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (global-company-mode t))

(use-package company-box
  :ensure t
  :defer t
  :hook (company-mode . company-box-mode))

(use-package company-coq
  :ensure t
  :defer t
  :after proof-general
  :hook (coq-mode . company-coq-mode))

(use-package vterm
  :ensure t
  :defer t)

(use-package vterm-toggle
  :ensure t
  :defer t)

(use-package multi-vterm
  :ensure t
  :defer t)
