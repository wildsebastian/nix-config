{ mkDerivation, aeson, ansi-terminal, attoparsec, base
, base64-bytestring, binary, blaze-html, bytestring
, case-insensitive, colour, containers, criterion, Diff, directory
, fetchgit, filepath, HUnit, hxt, mtl, pretty-show, QuickCheck
, random, regex-pcre-builtin, safe, stdenv, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, text, transformers, utf8-string
}:
mkDerivation {
  pname = "skylighting-core";
  version = "0.8.5";
  src = fetchgit {
    url = "git@github.com:jgm/skylighting.git";
    sha256 = "1nnqa7w4fvwydlyjlls38gvv2lz2cm0qm9f980dnfhf2jq26m5jl";
    rev = "ca52065a3992103ae97fa264f7efcc63d9450b0c";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/skylighting-core; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal attoparsec base base64-bytestring binary
    blaze-html bytestring case-insensitive colour containers directory
    filepath hxt mtl regex-pcre-builtin safe text transformers
    utf8-string
  ];
  testHaskellDepends = [
    aeson base bytestring containers Diff directory filepath HUnit
    pretty-show QuickCheck random tasty tasty-golden tasty-hunit
    tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    base containers criterion directory filepath text
  ];
  homepage = "https://github.com/jgm/skylighting";
  description = "syntax highlighting library";
  license = stdenv.lib.licenses.bsd3;
}
