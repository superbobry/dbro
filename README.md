# dbro

This is a very basic **unfinished** prototype of a relational database in Haskell.
No working tests. No user documentation. Almost zero code documentation. Sorry.

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
CREATE TABLE test(int foo, double bar);
OK
INSERT INTO test VALUES (1, 42.0);
OK 1
SELECT bar * foo FROM test;
42.0
```

Please, follow the [**style guide**](https://github.com/tibbe/haskell-style-guide)!
