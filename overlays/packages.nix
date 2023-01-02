self: super:
{
  procs = super.procs.overrideDerivation (attrs: {
    buildInputs = attrs.buildInputs ++ [ super.libiconv ];
  });

  proj = super.proj.overrideDerivation (attrs: {
    doCheck = false;
  });

  # nix = super.nix.overrideDerivation (attrs: {
  #   doCheck = false;
  # });
}
