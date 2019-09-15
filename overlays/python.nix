self: super:
{
  python = super.python // {
    packageOverrides = selfP: superP: rec {
      nose-randomly = superP.nose-randomly.overridePythonAttrs(old: rec {
        doCheck = false;
      });
    };
  };
}
