{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Firefox";
  version = "110.0";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-US/${pname}%20${version}.dmg";
    sha256 = "sha256-MFFh3WRWWoFvIqpvLoB/E5ozyK/N7OysoMfaDNO7OqQ=";
  };

  buildInputs = [ undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/Applications
    cp -r Firefox*.app "$out/Applications/"
  '';

  meta = {
    description = "Get the browser that protects what’s important";
    homepage = "https://www.mozilla.org/en-US/firefox/new/";
  };
}
