{ stdenv, lib, buildPythonPackage, isPyPy, fetchPypi, libheif, openssl }:

buildPythonPackage rec {
  pname = "pyheif";
  version = "0.5.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "fb23f6c71107c37fd667cb4ea363ddeb936b348bbd6449278eb92c189699f543";
  };

  nativeBuildInputs = [ libheif ];

  doCheck = false;

  meta = with lib; {
    description = "libheif";
    license = with licenses; [ apache2 ];
  };
}
