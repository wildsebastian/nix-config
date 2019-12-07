self: super:
{
  nano = super.nano.overrideAttrs (old: rec {
    version = "4.6-dev";
    src = super.fetchgit {
      url = "https://git.savannah.gnu.org/git/nano.git";
      rev = "f516cddce749c3bf938271ef3182b9169ac8cbcc";
      sha256 = "1dahpkiw406whkanm8p55zlj3qdnhbqjpss68fl983xcg6q0qzyh";
    };
  });
}
