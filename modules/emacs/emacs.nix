{ pkgs ? import <nixpkgs> { } }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./emacs.el;
  package = pkgs.emacsUnstable;
  alwaysEnsure = false;
  alwaysTangle = false;

  extraEmacsPackages = epkgs: [
    epkgs.agda2-mode
    (epkgs.emacsql.overrideAttrs (old: {
      buildInputs = old.buildInputs ++ [ pkgs.sqlite ];

      postBuild = ''
        cd source/sqlite
        make
        cd -
      '';

      postInstall = (old.postInstall or "") + "\n" + ''
        install -m=755 -D source/sqlite/emacsql-sqlite \
          $out/share/emacs/site-lisp/elpa/emacsql-${old.version}/sqlite/emacsql-sqlite
      '';

      stripDebugList = [ "share" ];
    }))
    epkgs.idris2-mode
  ];
}
