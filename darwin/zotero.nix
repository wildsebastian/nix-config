{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Zotero";
  # https://www.zotero.org/support/changelog
  version = "6.0.27";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download.zotero.org/client/release/${version}/${name}";
    sha256 = "sha256-ZtNMFBZ7CGp4O/xceB+2JDuPZ3FPdcuOkP8nHYIa3FE=";
  };

  buildInputs = [ undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Zotero*.app "$out/Applications/"
  '';

  meta = {
    description = "Zotero - Your personal research assistant";
    homepage = "https://www.zotero.org/";
  };
}
