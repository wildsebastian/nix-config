{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacsUnstable;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  myEmacsConfig = pkgs.writeText "default.el" (builtins.readFile ./emacs.el);
in
emacsWithPackages (epkgs: (
  with epkgs.melpaPackages; with epkgs.elpaPackages; [
    (pkgs.runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${myEmacsConfig} $out/share/emacs/site-lisp/default.el
      ''
    )
    all-the-icons
    base16-theme
    company
    company-box
    company-coq
    counsel
    dap-mode
    dashboard
    deft
    direnv
    editorconfig
    vterm
    evil
    evil-collection
    fill-column-indicator
    flycheck
    flycheck-haskell
    forge
    format-all
    fzf
    haskell-mode
    idris-mode
    lsp-haskell
    lsp-mode
    lsp-pyright
    lsp-ui
    lsp-treemacs
    magit
    markdown-mode
    nix-mode
    page-break-lines
    php-mode
    projectile
    proof-general
    psc-ide
    purescript-mode
    rg
    scss-mode
    swiper
    terraform-mode
    transient
    tramp
    treemacs
    treemacs-evil
    treemacs-magit
    treemacs-projectile
    typescript-mode
    use-package
    which-key
    web-mode
    yaml-mode
    zenburn-theme
  ]
))
