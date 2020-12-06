{ stdenv, buildPythonPackage, fetchPypi }:

buildPythonPackage rec {
  pname = "patiencediff";
  version = "0.2.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0nm2242xgg59l06m54rzxp41aly3lxjh0a1s7h6dk7ryxjh002lv";
  };

  # too complicated to setup
  doCheck = false;

  meta = with stdenv.lib; {
    description = "This package contains the implementation of the patiencediff algorithm";
    homepage = "https://www.breezy-vcs.org/";
  };
}
