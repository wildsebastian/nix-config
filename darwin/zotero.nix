{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Zotero";
  version = "6.0.19";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download.zotero.org/client/release/${version}/${name}";
    sha256 = "cac0dda0cd940778f8e3142f37a460229d590ba722fbe46a1089bc95cff27dd6";
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
