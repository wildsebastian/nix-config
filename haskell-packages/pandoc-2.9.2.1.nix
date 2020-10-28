{ mkDerivation, aeson, aeson-pretty, attoparsec, base-compat
, base-noprelude, base64-bytestring, binary, blaze-html
, blaze-markup, bytestring, case-insensitive, cmark-gfm, containers
, criterion, data-default, deepseq, Diff, directory, doclayout
, doctemplates, emojis, exceptions, executable-path, fetchgit
, filepath, Glob, haddock-library, hslua, hslua-module-system
, hslua-module-text, HsYAML, HTTP, http-client, http-client-tls
, http-types, ipynb, jira-wiki-markup, JuicyPixels, mtl, network
, network-uri, pandoc-types, parsec, process, QuickCheck, random
, safe, scientific, SHA, skylighting, skylighting-core, split
, stdenv, syb, tagsoup, tasty, tasty-golden, tasty-hunit, tasty-lua
, tasty-quickcheck, temporary, texmath, text, text-conversions
, time, unicode-transforms, unix, unordered-containers, vector
, weigh, xml, zip-archive, zlib
}:
mkDerivation {
  pname = "pandoc";
  version = "2.9.2.1";
  src = fetchgit {
    url = "git@github.com:jgm/pandoc.git";
    sha256 = "0x4szrcfksc80gbjrkd5pkhl2wvmljxwiz1dr683xm3xx4lrziya";
    rev = "adcce0c230bf3b44cf7c4bd8fcb461bb46a11bab";
    fetchSubmodules = true;
  };
  configureFlags = [ "-fhttps" "-f-trypandoc" ];
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base-compat base-noprelude
    base64-bytestring binary blaze-html blaze-markup bytestring
    case-insensitive cmark-gfm containers data-default deepseq
    directory doclayout doctemplates emojis exceptions filepath Glob
    haddock-library hslua hslua-module-system hslua-module-text HsYAML
    HTTP http-client http-client-tls http-types ipynb jira-wiki-markup
    JuicyPixels mtl network network-uri pandoc-types parsec process
    random safe scientific SHA skylighting skylighting-core split syb
    tagsoup temporary texmath text text-conversions time
    unicode-transforms unix unordered-containers vector xml zip-archive
    zlib
  ];
  executableHaskellDepends = [ base-compat base-noprelude ];
  testHaskellDepends = [
    base-compat base-noprelude base64-bytestring bytestring containers
    Diff directory doctemplates executable-path filepath Glob hslua mtl
    pandoc-types process QuickCheck tasty tasty-golden tasty-hunit
    tasty-lua tasty-quickcheck temporary text time xml zip-archive
  ];
  benchmarkHaskellDepends = [
    base-compat base-noprelude bytestring containers criterion mtl text
    time weigh
  ];
  postInstall = ''
    mkdir -p $out/share/man/man1
    mv "man/"*.1 $out/share/man/man1/
  '';
  homepage = "https://pandoc.org";
  description = "Conversion between markup formats";
  license = stdenv.lib.licenses.gpl2;
}
