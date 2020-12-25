{ mkDerivation, array, base, bytestring, containers, deepseq, dlist
, fetchgit, fmlist, HUnit, QuickCheck, random, stdenv, text
, utf8-string, vector
}:
mkDerivation {
  pname = "ListLike";
  version = "4.7.2";
  src = fetchgit {
    url = "https://github.com/ddssff/listlike.git";
    sha256 = "0976n25lmfjn3d4c0ngc4v421mswmghfvkxsw3amsmpkj54pw410";
    rev = "c6ef5ceca4fe94c002213f463ecb2f42f3572c58";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base bytestring containers deepseq dlist fmlist text
    utf8-string vector
  ];
  testHaskellDepends = [
    array base bytestring containers dlist fmlist HUnit QuickCheck
    random text utf8-string vector
  ];
  homepage = "http://github.com/ddssff/listlike";
  description = "Generalized support for list-like structures";
  license = stdenv.lib.licenses.bsd3;
}
