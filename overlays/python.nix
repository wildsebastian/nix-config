self: super:
rec {
  # nix-shell -p python.pkgs.my_stuff
  python37 = super.python37.override {
    # Careful, we're using a different self and super here!
    packageOverrides = self: super: {
      tinycss2 = super.tinycss2.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      python-language-server = super.python-language-server.overridePythonAttrs(old: rec {
        doCheck = false;
      });
    };
  };
  # nix-shell -p pythonPackages.my_stuff
  python37Packages = python37.pkgs;
}
