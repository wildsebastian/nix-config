{ mkDerivation
, aeson
, aeson-pretty
, attoparsec
, base
, base64-bytestring
, binary
, blaze-html
, blaze-markup
, bytestring
, case-insensitive
, citeproc
, commonmark
, commonmark-extensions
, commonmark-pandoc
, connection
, containers
, criterion
, data-default
, deepseq
, Diff
, directory
, doclayout
, doctemplates
, emojis
, exceptions
, executable-path
, fetchgit
, file-embed
, filepath
, Glob
, haddock-library
, hslua
, hslua-module-system
, hslua-module-text
, HsYAML
, HTTP
, http-client
, http-client-tls
, http-types
, ipynb
, jira-wiki-markup
, JuicyPixels
, lib
, mtl
, network
, network-uri
, pandoc-types
, parsec
, process
, QuickCheck
, random
, safe
, scientific
, SHA
, skylighting
, skylighting-core
, split
, syb
, tagsoup
, tasty
, tasty-golden
, tasty-hunit
, tasty-lua
, tasty-quickcheck
, temporary
, texmath
, text
, text-conversions
, time
, unicode-transforms
, unix
, unordered-containers
, weigh
, xml
, zip-archive
, zlib
}:
mkDerivation {
  pname = "pandoc";
  version = "2.11.4";
  src = fetchgit {
    url = "https://github.com/jgm/pandoc.git";
    sha256 = "0j4hfm0wmld303n4vabvzkrk27lfk04d7a3lwy0q2dxgxkxkypjh";
    rev = "54d8c6959cc53e78cfea2093b33504672f81ed74";
    fetchSubmodules = true;
  };
  configureFlags = [ "-fhttps" "-f-trypandoc" ];
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson
    aeson-pretty
    attoparsec
    base
    base64-bytestring
    binary
    blaze-html
    blaze-markup
    bytestring
    case-insensitive
    citeproc
    commonmark
    commonmark-extensions
    commonmark-pandoc
    connection
    containers
    data-default
    deepseq
    directory
    doclayout
    doctemplates
    emojis
    exceptions
    file-embed
    filepath
    Glob
    haddock-library
    hslua
    hslua-module-system
    hslua-module-text
    HsYAML
    HTTP
    http-client
    http-client-tls
    http-types
    ipynb
    jira-wiki-markup
    JuicyPixels
    mtl
    network
    network-uri
    pandoc-types
    parsec
    process
    random
    safe
    scientific
    SHA
    skylighting
    skylighting-core
    split
    syb
    tagsoup
    temporary
    texmath
    text
    text-conversions
    time
    unicode-transforms
    unix
    unordered-containers
    xml
    zip-archive
    zlib
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base
    base64-bytestring
    bytestring
    containers
    Diff
    directory
    doctemplates
    exceptions
    executable-path
    filepath
    Glob
    hslua
    mtl
    pandoc-types
    process
    QuickCheck
    tasty
    tasty-golden
    tasty-hunit
    tasty-lua
    tasty-quickcheck
    temporary
    text
    time
    xml
    zip-archive
  ];
  benchmarkHaskellDepends = [
    base
    bytestring
    containers
    criterion
    mtl
    text
    time
    weigh
  ];
  postInstall = ''
    mkdir -p $out/share/man/man1
    mv "man/"*.1 $out/share/man/man1/
  '';
  homepage = "https://pandoc.org";
  description = "Conversion between markup formats";
  license = lib.licenses.gpl2Plus;
}
