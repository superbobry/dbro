# dbro [![Build Status](https://secure.travis-ci.org/superbobry/dbro.png)](http://travis-ci.org/superbobry/dbro)#

Development
-----------

Make sure you have **at least** GHC 7.4.2, then just do the usual
[cabal-dev](http://hackage.haskell.org/package/cabal-dev) routine:

```bash
$ cabal-dev install-deps --enable-tests
$ cabal-dev configure -fdebug --enable-tests
$ cabal-dev build
```

After compilation, `dbro` binary will be available under `dist/build/dbro`:

```bash
$ dist/build/dbro/dbro
select * from foo;
Done "" SelectAll "foo"
```

Please, follow the [**style guide**](https://github.com/tibbe/haskell-style-guide)!
