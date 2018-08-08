pkgs: version: hpkgs: with hpkgs;

# These work with every version of GHC environments are created for
[
  aeson
  amqp
  async
  base
  base-encoding
  base64-bytestring
  bytestring
  conduit
  conduit-extra
  containers
  contravariant
  criterion
  cryptonite
  cryptonite-conduit
  fast-logger
  ghc
  hasql
  hasql-transaction
  hasql-pool
  hasql-th
  hedgehog
  hspec
  hspec-wai
  hspec-wai-json
  http-client
  http-client-tls
  json
  lens
  mtl
  monad-logger
  persistent
  persistent-mysql
  persistent-postgresql
  persistent-template
  pipes
  pipes-concurrency
  pipes-csv
  pipes-group
  pipes-parse
  pipes-safe
  postgresql-binary
  postgresql-simple
  QuickCheck
  servant
  servant-auth
  servant-auth-server
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
  uuid
  vector
  wai
  wai-logger
  warp
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

