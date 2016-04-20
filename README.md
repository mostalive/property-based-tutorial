# property-based-tutorial

Workshop materials and experiments for property based testing tutorial
at the Joy of Coding 2015 and ACCU 2016 conferences.

The tutorial comes in two flavours:
- Javascript - because virtually everyone has to use it these days
- Haskell - because that is where property-based testing really gained traction with QuickCheck

## Current status

See [Changelog](Changelog)

## Installation options

There are several options available for doing this tutorial:
- work with a NodeJS or Haskell installation on your machine
 - work in the cloud (Javascript only)

### Local NodeJS or Haskell

Clone this repository 
`git clone https://github.com/qwaneu/property-based-tutorial.git`

#### Javascript

The Javascript tutorial uses the jsverify library, one of the available
Javascript property based testing libraries.

Provided you have Node and NPM installed:

```
cd exercises/js
npm install jsverify
npm install underscore
```

#### Haskell

[Install Stack](https://github.com/commercialhaskell/stack)

```
cd exercises/hsmoney
stack build
```

Run the tests with
```
stack build joy && stack exec joy 
```

Or use the repl, see the [detailed README](exercises/hsmoney/README.md)
for instructions.


### Running Javascript in the cloud

Cloud9 provides a development environment in the cloud: https://c9.io/

You can sign up; it provides a development environment including
editors, a terminal, NodeJS and Git support. 

Sign in / Log in, open a terminal, and clone this repository:
`git clone https://github.com/qwaneu/property-based-tutorial.git`

Install the required Node modules:
```
cd property-based-tutorial/exercises/js
npm install jsverify
npm install underscore
```

# Property Based Testing - Introduction

In practice we usually do _example based testing_. We write an example
of how a function or system under test behaves. Examples can communicate
very well. It might be difficult to catch all corner cases. And with
example based test, we try to find a minimal set of examples, to keep
the maintenance burden low.

Example based testing provides just a few data points to capture the
system under test. How do you know that you've actually caught those
pesky corner cases in your examples?

Property based testing takes a different approach: 

- you define properties - invariants of the system-under-test
- the property based testing library generate lots of random input data
- the system under test is run with this input data and the testing
library verifies that the invariants hold under all inputs

## Benefits

In our experience, property based testing can be beneficial in several
cases:

- Characterization testing - understand existing libraries, quickly test a
wide range of cases (e.g. numeric types or libraries to represent
money).
- Testing validation logic
- Testing mappings / adapters
- Finding pesky corner cases
- When example based testing gets repetitive and/or you want to check many
cases, and there is no obvious way to express the variations succinctly.
Or when your examples don’t express your intent very well, no matter how
hard you try.

Furthermore, Property Based Testing drives your design, something it has
in common with Test Driven Development (although it drives your design
in a different way). Property based testing forces you to think about
invariant properties of your code and capture these explicitly. Defining
custom generators helps you get a better understanding of the
preconditions of your code.

Property based testing is a relatively new practice. We are still
learning about its applicability and usefulness.

## Key concepts

_Properties_: invariants of the code under test; invariants should always be true
under all valid inputs

_Arbitraries & generators_: A generator is a function that can generate arbitrary data of a specific
type. An arbitrary combines a generator with a shrinking function. 

_Shrinking_: Shrinking is used when a counter example is found, to reduce the counter
example to the smallest one that still fails the property

## I want this in my favorite language!

Property based testing is available for most programming languages,
like:

- Haskell: Quickcheck
- Scala: ScalaCheck
- Java: https://github.com/pholser/junit-quickcheck
- Javascript: many libraries available, like _jsverify_ and _claire_
- C# / .Net: FsCheck
- Elm: http://package.elm-lang.org/packages/TheSeamau5/elm-check/3.0.1

