{ mkDerivation, aeson, base, bytestring, containers, criterion
, deepseq, fetchgit, ghc-prim, HUnit, QuickCheck, stdenv, string-qq
, syb, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, transformers
}:
mkDerivation {
  pname = "pandoc-types";
  version = "1.20";
  src = fetchgit {
    url = "git@github.com:jgm/pandoc-types.git";
    sha256 = "0yd4g5kj4dpxhqgc8lgwyhxvdd7jnzzmmwfsvvdbjkmhdzi0zvbx";
    rev = "6817d99d5ce151a6c4a7a4d69215bdd929aacaa0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq ghc-prim QuickCheck syb
    text transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers HUnit QuickCheck string-qq syb
    test-framework test-framework-hunit test-framework-quickcheck2 text
  ];
  benchmarkHaskellDepends = [ base criterion text ];
  homepage = "https://pandoc.org/";
  description = "Types for representing a structured document";
  license = stdenv.lib.licenses.bsd3;
}
