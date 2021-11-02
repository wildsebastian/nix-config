self: super:
{
  procs = super.procs.overrideDerivation (attrs: {
    buildInputs = attrs.buildInputs ++ [ super.libiconv ];
  });

  aws-sdk-cpp = super.aws-sdk-cpp.overrideDerivation (attrs: {
    src = super.fetchFromGitHub {
      owner = "awslabs";
      repo = "aws-sdk-cpp";
      rev = "1.8.130";
      sha256 = "sha256-5T4l0KYB0utFTdEOtYT9trQ/JehQbXxk/IhI6YavErs=";
    };
    buildInputs = with self.pkgs; [
      curl openssl zlib
    ] ++ lib.optionals (stdenv.isDarwin &&
                        ((builtins.elem "text-to-speech" ["*"]) ||
                         (builtins.elem "*" ["*"])))
            [ darwin.apple_sdk.frameworks.CoreAudio darwin.apple_sdk.frameworks.AudioToolbox ];
  });

  proj = super.proj.overrideDerivation (attrs: {
    doCheck = false;
  });

  nix = super.nix.overrideDerivation (attrs: {
    doCheck = false;
  });
}
