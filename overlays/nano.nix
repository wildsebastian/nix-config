self: super: {
  nano = with self; super.stdenv.mkDerivation rec {
    pname = "nano";
    version = "4.6";

    src = fetchurl {
      url = "mirror://gnu/nano/${pname}-${version}.tar.xz";
      sha256 = "1s98jsvkfar6qmd5n5l1n1k59623dnc93ciyvlhxjkvpad0kmb4v";
    };

    nativeBuildInputs = [ texinfo ] ++ stdenv.lib.optional false gettext;
    buildInputs = [ ncurses ];

    patches = [ ./nano_mac.patch ];

    outputs = [ "out" "info" ];

    configureFlags = [
      "--sysconfdir=/etc"
      (stdenv.lib.enableFeature false "nls")
      (stdenv.lib.enableFeature false "tiny")
    ];

    enableParallelBuilding = true;

    meta = with stdenv.lib; {
      homepage = https://www.nano-editor.org/;
      description = "A small, user-friendly console text editor";
      license = licenses.gpl3Plus;
      maintainers = with maintainers; [
        joachifm
      ];
      platforms = platforms.all;
    };
  };
}
