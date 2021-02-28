{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacsGcc;
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
    centaur-tabs
    company
    company-box
    company-coq
    counsel
    dap-mode
    dashboard
    deft
    direnv
    docker
    doom-modeline
    editorconfig
    vterm
    evil
    evil-collection
    flycheck
    flycheck-haskell
    forge
    format-all
    fzf
    ghub
    haskell-mode
    idris-mode
    ivy
    ivy-yasnippet
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
    undo-tree
    use-package
    which-key
    web-mode
    yaml-mode
    yasnippet
    yasnippet-snippets
    zenburn-theme
  ]
))
