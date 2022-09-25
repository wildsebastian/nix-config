{ mkDerivation
, base
, bytestring
, containers
, directory
, exceptions
, fetchgit
, filepath
, HUnit
, process
, stdenv
, stm
, terminfo
, text
, transformers
, unix
}:
mkDerivation {
  pname = "haskeline";
  version = "0.8.1.0";
  src = fetchgit {
    url = "https://github.com/judah/haskeline.git";
    sha256 = "1lr2lvc4ykn128jbirk26hpa0rlgyk3f1b9g9jrzy2x9nz8fdpxh";
    rev = "28ee26ad5b4ae1c0584f2ec11ac53be9671bf878";
    fetchSubmodules = true;
  };
  configureFlags = [ "-fterminfo" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    bytestring
    containers
    directory
    exceptions
    filepath
    process
    stm
    terminfo
    transformers
    unix
  ];
  executableHaskellDepends = [ base containers ];
  testHaskellDepends = [
    base
    bytestring
    containers
    HUnit
    process
    text
    unix
  ];
  homepage = "https://github.com/judah/haskeline";
  description = "A command-line interface for user input, written in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
