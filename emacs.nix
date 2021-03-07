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
    auto-yasnippet
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
    elfeed
    evil
    evil-collection
    flycheck
    flycheck-haskell
    forge
    format-all
    fzf
    ghub
    git-gutter
    git-gutter-fringe
    haskell-mode
    idris-mode
    ivy
    ivy-yasnippet
    lsp-haskell
    lsp-ivy
    lsp-mode
    lsp-pyright
    lsp-treemacs
    magit
    markdown-mode
    nix-mode
    org
    page-break-lines
    perspective
    php-mode
    projectile
    proof-general
    psc-ide
    purescript-mode
    restclient
    rg
    scss-mode
    swiper
    terraform-mode
    tramp
    transient
    treemacs
    treemacs-evil
    treemacs-magit
    treemacs-projectile
    typescript-mode
    undo-tree
    use-package
    vterm
    web-mode
    which-key
    yaml-mode
    yasnippet
    yasnippet-snippets
  ]
))
