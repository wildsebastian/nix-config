pkgs: version: hpkgs: with hpkgs;

# These work with every version of GHC environments are created for
[
  aeson
  base
  bytestring
  containers
  criterion
  ghc
  hedgehog
  hspec
  lens
  mtl
  QuickCheck
  servant
  servant-auth
  servant-client
  servant-server
  tasty
  tasty-hunit
  tasty-quickcheck
  tasty-th
  template-haskell
  text
  time
  transformers
  unordered-containers
  vector
  xml
] ++

# These don't work with GHC 8.4 yet
(pkgs.stdenv.lib.optionals (version < 8.3)
[
  diagrams
  diagrams-core
  diagrams-graphviz
  diagrams-lib
  diagrams-svg
]) ++

# These only work with GHC 8.2
(pkgs.stdenv.lib.optionals (version > 8.1 && version < 8.3)
[
  liquidhaskell
])

