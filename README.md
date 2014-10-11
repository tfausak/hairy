# [Hairy][1]

[![Package version][2]][3]
[![Build status][4]][5]
[![Dependency status][6]][7]

Hairy is a JSON REST API built in Haskell. It uses Scotty to create a WAI
application served by the Warp web server. The Aeson library handles encoding
and decoding JSON, while Persistent manages the PostgreSQL database. Hairy
features an Hspec test suite and Criterion benchmarks.

The entire project is meant to be easy to build, understand, and use. It doesn't
really do anything useful, but it shows how to do anything at all. Take a look
at [the literate source][8] to see how it all works.

## Installation

Add it to your Cabal file:

```
library
  build-depends:
    hairy ==0.1.*
```

Or install it manually:

``` sh
$ cabal update
$ cabal install hairy-0.1.1
```

This package uses [Semantic Versioning][9].

[1]: https://github.com/tfausak/hairy
[2]: https://img.shields.io/hackage/v/hairy.svg?style=flat
[3]: https://hackage.haskell.org/package/hairy
[4]: https://img.shields.io/travis/tfausak/hairy/master.svg?style=flat
[5]: https://travis-ci.org/tfausak/hairy
[6]: https://img.shields.io/hackage-deps/v/hairy.svg?style=flat
[7]: http://packdeps.haskellers.com/feed?needle=hairy
[9]: ./library/Hairy.lhs
[9]: http://semver.org/spec/v2.0.0.html
