{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Zotero";
  # https://www.zotero.org/support/changelog
  version = "6.0.23";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download.zotero.org/client/release/${version}/${name}";
    sha256 = "sha256-euWgmo1AsP0Mj/y8Cl8NMx3SlYU+WLbDBW6z1wdkafU=";
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
