{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Zotero";
  # https://www.zotero.org/support/changelog
  version = "7.0.3";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download.zotero.org/client/release/${version}/${name}";
    sha256 = "sha256-sFDwp3YSLBFMIrp+8OBDDFJKj7GJ3WjMc2J2EqPRQNU=";
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
