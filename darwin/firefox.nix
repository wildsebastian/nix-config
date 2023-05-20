{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Firefox";
  # https://product-details.mozilla.org/1.0/firefox_versions.json
  version = "113.0.1";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-US/${pname}%20${version}.dmg";
    # https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/SHA256SUMS
    sha256 = "9556a573435887378246e9df6ca27f719f7690f9a89155a4f2b5607c4b8240b9";
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
