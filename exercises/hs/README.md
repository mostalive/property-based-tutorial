% Property Based Testing Hands On - Haskell introduction
% (c) QWAN - Quality Without a Name - www.qwan.eu
% May 2015

- Willem van den Ende - willem@qwan.eu
- Marc Evers - marc@qwan.eu
- Rob Westgeest - rob@qwan.eu


# Prerequisites

We recommend [installing
stack](https://github.com/commercialhaskell/stack) to build the exercise
code, so that you can build this tutorial with the exact same set of
packags and the exact same compiler we used.

We use the
__[QuickCheck](https://hackage.haskell.org/package/QuickCheck)__ library
in this tutorial. You can find out the exact version used at any time by
checking stack.yaml

The `src` directory contains an entrypoint `src/joy.hs` and a module
`src/GettingStarted.hs` to do the first exercise in. 

Building and running:

```bash
stack build && stack exec joy
```

Interactive exploring, edit e.g. `src/MyCoolModule.hs`  (does not have to be in the
`.cabal` file) and run:

```bash
stack ghci joy
>ghci :l MyCoolModuleInSrc
```

You can opt to just add new modules to `hsmoney.cabal` under the `joy`
executable so you can build a suite of properties and do it as you go.

