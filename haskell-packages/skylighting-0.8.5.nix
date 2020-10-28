{ mkDerivation, base, binary, bytestring, containers, fetchgit
, skylighting-core, stdenv
}:
mkDerivation {
  pname = "skylighting";
  version = "0.8.5";
  src = fetchgit {
    url = "git@github.com:jgm/skylighting.git";
    sha256 = "1nnqa7w4fvwydlyjlls38gvv2lz2cm0qm9f980dnfhf2jq26m5jl";
    rev = "ca52065a3992103ae97fa264f7efcc63d9450b0c";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/skylighting; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers skylighting-core
  ];
  homepage = "https://github.com/jgm/skylighting";
  description = "syntax highlighting library";
  license = stdenv.lib.licenses.gpl2;
}
