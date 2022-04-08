self: super:

{
  python38 = super.python38.override {
    # Careful, we're using a different self and super here!
    packageOverrides = self: super: {
      django_silk = super.django_silk.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      python-language-server = super.python-language-server.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      pylint-django = super.pylint-django.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      httplib2 = super.httplib2.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      pycairo = super.pycairo.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      pycurl = super.pycurl.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      weasyprint = super.weasyprint.overridePythonAttrs(old: rec {
        doCheck = false;
      });
    };
  };
  python39 = super.python39.override {
    # Careful, we're using a different self and super here!
    packageOverrides = self: super: {
      django_silk = super.django_silk.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      httplib2 = super.httplib2.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      keyring = super.keyring.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      pycairo = super.pycairo.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      pylint-django = super.pylint-django.overridePythonAttrs(old: rec {
        doCheck = false;
      });
      weasyprint = super.weasyprint.overridePythonAttrs(old: rec {
        doCheck = false;
      });
    };
  };
}
