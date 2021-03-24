self: super:
{
  procs = super.procs.overrideDerivation (attrs: {
    buildInputs = attrs.buildInputs ++ [ super.libiconv ];
  });
}
