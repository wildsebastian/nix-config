{ mkDerivation
, aeson
, base
, bytestring
, containers
, criterion
, deepseq
, fetchgit
, ghc-prim
, HUnit
, QuickCheck
, stdenv
, string-qq
, syb
, test-framework
, test-framework-hunit
, test-framework-quickcheck2
, text
, transformers
}:
mkDerivation {
  pname = "pandoc-types";
  version = "1.21";
  src = fetchgit {
    url = "https://github.com/jgm/pandoc-types.git";
    sha256 = "196sns7z57h2bi84f7q7zg6j2zm05fksdym9a1pnmsd9c045588i";
    rev = "1f4b96bfd78b6dc885b52e0248a20d2e860424a5";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    containers
    deepseq
    ghc-prim
    QuickCheck
    syb
    text
    transformers
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    containers
    HUnit
    QuickCheck
    string-qq
    syb
    test-framework
    test-framework-hunit
    test-framework-quickcheck2
    text
  ];
  benchmarkHaskellDepends = [ base criterion text ];
  homepage = "https://pandoc.org/";
  description = "Types for representing a structured document";
  license = stdenv.lib.licenses.bsd3;
}
