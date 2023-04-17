{ stdenv, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Firefox";
  # https://product-details.mozilla.org/1.0/firefox_versions.json
  version = "112.0.1";

  src = fetchurl rec {
    name = "${pname}-${version}.dmg";
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-US/${pname}%20${version}.dmg";
    # https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/SHA256SUMS
    sha256 = "32552a49608497638130efd39465df9f2ced5b8235966a262665c52bf7823b1f";
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
