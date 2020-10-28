{ mkDerivation, base, bytestring, containers, criterion, deepseq
, exceptions, fetchgit, lua5_3, mtl, QuickCheck
, quickcheck-instances, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "hslua";
  version = "1.0.3.2";
  src = fetchgit {
    url = "git@github.com:hslua/hslua.git";
    sha256 = "1i4xdwj3qkmc0z47pna4dqz5jspi2504hd44qnzasxjlfqy2gc83";
    rev = "118a8be57a3d7cb64ecf7cbf0196be27774cce9f";
    fetchSubmodules = true;
  };
  configureFlags = [ "-fsystem-lua" "-f-use-pkgconfig" ];
  libraryHaskellDepends = [
    base bytestring containers exceptions mtl text
  ];
  librarySystemDepends = [ lua5_3 ];
  testHaskellDepends = [
    base bytestring containers exceptions mtl QuickCheck
    quickcheck-instances tasty tasty-hunit tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [ base bytestring criterion deepseq ];
  homepage = "https://hslua.github.io/";
  description = "Bindings to Lua, an embeddable scripting language";
  license = stdenv.lib.licenses.mit;
}
