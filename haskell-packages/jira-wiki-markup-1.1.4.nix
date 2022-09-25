{ mkDerivation
, base
, fetchgit
, mtl
, parsec
, stdenv
, tasty
, tasty-hunit
, text
}:
mkDerivation {
  pname = "jira-wiki-markup";
  version = "1.1.4";
  src = fetchgit {
    url = "https://github.com/tarleb/jira-wiki-markup.git";
    sha256 = "1qgcf4sc3v8rwaraff2wfp4k5js38p2d32i5hrigljq2f053lc93";
    rev = "875a45f4cd15caca75b4f30e53a29546247d0781";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl parsec text ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [ base parsec tasty tasty-hunit text ];
  homepage = "https://github.com/tarleb/jira-wiki-markup";
  description = "Handle Jira wiki markup";
  license = stdenv.lib.licenses.mit;
}
