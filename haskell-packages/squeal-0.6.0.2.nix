{ mkDerivation, aeson, async, base, binary, binary-parser
, bytestring, bytestring-strict-builder, deepseq, doctest, fetchgit
, free-categories, gauge, generic-random, generics-sop, hedgehog
, hspec, mmorph, monad-loops, mtl, network-ip, postgresql-binary
, postgresql-libpq, profunctors, QuickCheck, quickcheck-instances
, records-sop, resource-pool, scientific, stdenv, text, time
, transformers, unliftio, unliftio-pool, uuid-types, vector
, with-utf8
}:
mkDerivation {
  pname = "squeal-postgresql";
  version = "0.6.0.2";
  src = fetchgit {
    url = "git@github.com:morphismtech/squeal.git";
    sha256 = "1abahfyfhf2q456246xw76j6kfphyfby877acjd2zp20xqfa6i0l";
    rev = "7557c2ddd32430ed82937a7864e5879b1ac77513";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/squeal-postgresql; echo source root reset to $sourceRoot";
  doCheck = false;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary binary-parser bytestring
    bytestring-strict-builder deepseq free-categories generics-sop
    mmorph mtl network-ip postgresql-binary postgresql-libpq
    profunctors records-sop resource-pool scientific text time
    transformers unliftio unliftio-pool uuid-types vector
  ];
  executableHaskellDepends = [
    base bytestring generics-sop mtl text transformers vector
  ];
  testHaskellDepends = [
    async base bytestring doctest generics-sop hedgehog hspec mtl
    scientific text time vector with-utf8
  ];
  benchmarkHaskellDepends = [
    base bytestring deepseq gauge generic-random generics-sop
    monad-loops mtl QuickCheck quickcheck-instances scientific text
    with-utf8
  ];
  homepage = "https://github.com/morphismtech/squeal";
  description = "Squeal PostgreSQL Library";
  license = stdenv.lib.licenses.bsd3;
}
