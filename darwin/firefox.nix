{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Firefox";
  # https://product-details.mozilla.org/1.0/firefox_versions.json
  version = "114.0";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-US/${pname}%20${version}.dmg";
    # https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/SHA256SUMS
    sha256 = "f0c58ec0966283be8581c874d86ac3f6eac34ca3da9eafdbb417ad021b76979a";
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
