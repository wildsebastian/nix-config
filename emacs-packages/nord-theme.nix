{ stdenv, fetchurl, emacs }:

stdenv.mkDerivation {
  name = "nord-theme-20210109";

  dontUnpack = true;

  src = fetchurl {
    url = "https://raw.githubusercontent.com/wildsebastian/nord-emacs/emacs_daemon/nord-theme.el";
    sha256 = "d5eccba079c8636ad9fe1bba9a2c1361d8bf4ef5ef625a6fb61620e75328e1a4";
  };

  buildInputs = [ emacs ];

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    cp $src $out/share/emacs/site-lisp/nord-theme.el
  '';

  meta = {
    description = "My custom nord-theme";
    platforms = stdenv.lib.platforms.all;
  };
}
