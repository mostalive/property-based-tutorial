% Property Based Testing Hands On - Haskell introduction
% (c) QWAN - Quality Without a Name - www.qwan.eu
% May 2015

- Rob Westgeest - rob@qwan.eu
- Marc Evers - marc@qwan.eu
- Willem van den Ende - willem@qwan.eu


# Prerequisites

You need GHC 7.8.something, cabal and a terminal plus your favorite editor to do
the haskell exercise. 
We use the
__[QuickCheck](https://hackage.haskell.org/package/QuickCheck)__ library in this tutorial. There are many
libraries available in your favorite languages, but the basic principles are
the same.

In case you are running this on your machine, instead of at fpcomplete
or in the docker container provided, You need to install QuickCheck 7.8.1 or greater for these exercises: 

```bash
cabal install --only-dependencies
```

The `src` directory contains an entrypoint `src/joy.hs` and a module
`src/GettingStarted.hs` to do the first exercise in. 

Building and running:

```bash
cabal build && ./dist/build/joy/joy
```

Interactive exploring, edit e.g. `src/MyCoolModule.hs`  (does not have to be in the
`.cabal` file) and run:

```bash
cabal repl joy
>ghci :l MyCoolModuleInSrc
```

You can opt to just add new modules to `hsmoney.cabal` under the `joy`
executable so you can build a suite of properties and do it as you go.

TODO The remainder of the instructions will be in the doc/ directory. Check
back here for updates in case you installed early before joy of coding.
