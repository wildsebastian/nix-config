{ mkDerivation, base, binary, blaze-html, blaze-markup, bytestring
, containers, cryptonite, data-default, deepseq, directory
, fetchgit, file-embed, filepath, fsnotify, http-conduit
, http-types, lrucache, memory, mtl, network-uri
, optparse-applicative, pandoc, pandoc-citeproc, parsec, process
, QuickCheck, random, regex-tdfa, resourcet, scientific, stdenv
, tagsoup, tasty, tasty-hunit, tasty-quickcheck, template-haskell
, text, time, time-locale-compat, unordered-containers, utillinux
, vector, wai, wai-app-static, warp, yaml
}:
mkDerivation {
  pname = "hakyll";
  version = "4.13.4.1";
  src = fetchgit {
    url = "git://github.com/jaspervdj/hakyll.git";
    sha256 = "1662dc9ajr9d9llm1ykrcz3jg6pkh8rbhkjqpvpiyg5j46spjrxd";
    rev = "e9a8139152b166eae75083259fc3e824675de6fb";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base binary blaze-html blaze-markup bytestring containers
    cryptonite data-default deepseq directory file-embed filepath
    fsnotify http-conduit http-types lrucache memory mtl network-uri
    optparse-applicative pandoc pandoc-citeproc parsec process random
    regex-tdfa resourcet scientific tagsoup template-haskell text time
    time-locale-compat unordered-containers vector wai wai-app-static
    warp yaml
  ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base bytestring containers filepath QuickCheck tasty tasty-hunit
    tasty-quickcheck text unordered-containers yaml
  ];
  testToolDepends = [ utillinux ];
  doCheck = false;
  homepage = "http://jaspervdj.be/hakyll";
  description = "A static website compiler library";
  license = stdenv.lib.licenses.bsd3;
}
