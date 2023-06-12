{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Firefox";
  # https://product-details.mozilla.org/1.0/firefox_versions.json
  version = "114.0.1";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-US/${pname}%20${version}.dmg";
    # https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/SHA256SUMS
    sha256 = "dd6783c636ec5ec302fb7dc79e998552f18eedb4cdc1cb88e2c0b92a9757abbd";
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
