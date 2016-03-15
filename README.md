# [Hairy][]

[![Version badge][]][version]
[![Build badge][]][build]

Hairy is a JSON REST API built in Haskell. It uses Scotty to create a WAI
application served by the Warp web server. The Aeson library handles encoding
and decoding JSON, while Persistent manages the PostgreSQL database. Hairy
features an Hspec test suite and Criterion benchmarks.

The entire project is meant to be easy to build, understand, and use. It doesn't
really do anything useful, but it shows how to do anything at all. Take a look
at [the literate source][8] to see how it all works.

- [Requirements](#requirements)
- [Installation](#installation)
- [Setup](#setup)
- [Configuration](#configuration)

## Requirements

Hairy works best with the latest [Haskell Platform][9], but it also supports GHC
7.8 and 7.6. [PostgreSQL][10] 9.1 or later is also required.

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
$ cabal install hairy-0.1.3
```

This package uses [Semantic Versioning][11].

## Setup

First create a database user for Hairy.

``` sh
$ createuser --createdb postgres
```

Then create databases for each environment.

``` sh
$ psql --username postgres --command 'CREATE DATABASE hairy_development'
$ psql --username postgres --command 'CREATE DATABASE hairy_production'
$ psql --username postgres --command 'CREATE DATABASE hairy_test'
```

Then just start the server!

``` sh
$ hairy
# => http://localhost:3000
```

## Configuration

Hairy can be configured through environment variables.

To configure the environment, use the `SCOTTY_ENV` environment variable.
Possible environments include `Development`, `Production`, and `Test`.

``` sh
$ env SCOTTY_ENV=Production hairy
```

To configure the port, use the `PORT` environment variable.

``` sh
$ env PORT=8080 hairy
```

To configure the database, use the `DATABASE_URL` environment variable.

``` sh
$ env DATABASE_URL=postgres://postgres:postgres@localhost:5432/hairy_development hairy
```

[Hairy]: https://github.com/tfausak/hairy
[Version badge]: https://img.shields.io/hackage/v/hairy.svg
[version]: https://hackage.haskell.org/package/hairy
[Build badge]: https://img.shields.io/travis/tfausak/hairy/master.svg
[build]: https://travis-ci.org/tfausak/hairy
[8]: ./library/Hairy.lhs
[9]: https://www.haskell.org/platform/
[10]: http://www.postgresql.org
[11]: http://semver.org/spec/v2.0.0.html
