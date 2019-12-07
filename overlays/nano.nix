self: super:
{
  nano = super.nano.override {
    src = super.fetchurl {
      url = "https://git.savannah.gnu.org/cgit/nano.git/snapshot/nano-f517cddce749c3bf938271ef3182b9169ac8cbcc.tar.gz";
      sha256 = "1s98jsvkfar6qmd5n5l1n1k59623dnc93ciyvlhxjkvpad0kmb4v";
    };
  };
}
