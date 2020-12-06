{ stdenv, buildPythonPackage, fetchPypi }:

buildPythonPackage rec {
  pname = "patiencediff";
  version = "0.2.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "";
  };

  # too complicated to setup
  doCheck = false;

  meta = with stdenv.lib; {
    description = "This package contains the implementation of the patiencediff algorithm";
    homepage = "https://www.breezy-vcs.org/";
    license = licenses.gplv2;
  };
}
