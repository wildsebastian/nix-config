{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Zotero";
  # https://www.zotero.org/support/changelog
  version = "6.0.28";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download.zotero.org/client/release/${version}/${name}";
    sha256 = "0w5hv9rq382bq7r61k8wy52wyhjr73m092100kwy835cw9kkakvi";
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
