{ mkDerivation
, base
, bytestring
, directory
, fetchgit
, filepath
, stdenv
, template-haskell
}:
mkDerivation {
  pname = "file-embed";
  version = "0.0.11.2";
  src = fetchgit {
    url = "https://github.com/snoyberg/file-embed.git";
    sha256 = "0k7ha4ap3h8kq1miym6g7m43n6wairapjsx9vxw6l3pdi4k54c5c";
    rev = "739cef92b46b3b1a71b8d46d0de83c6bc49e3c23";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base
    bytestring
    directory
    filepath
    template-haskell
  ];
  testHaskellDepends = [ base filepath ];
  homepage = "https://github.com/snoyberg/file-embed";
  description = "Use Template Haskell to embed file contents directly";
  license = stdenv.lib.licenses.bsd3;
}
