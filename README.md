# property-based-tutorial

Workshop materials and experiments for property based testing tutorial
at the Joy of Coding 2015.

The tutorial comes in two flavours:
- Javascript - because virtually everyone has to use it these days
- Haskell - because that is where property-based testing really gained traction with QuickCheck

## Current status

This is a work in progress, check back shortly before the conference for
the final version. Your feedback on things that work and don't is
appreciated.

## Installation options

There are several options available for doing this tutorial:
- work with a NodeJS or Haskell installation on your machine
- use Docker to pull an image that we have prepared with all the
  Javascript and Haskell stuff installed
- work in the cloud

### You have a working NodeJS or Haskell installation on your machine

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

```
cd exercises/hsmoney
cabal install --only-dependencies
```

Run the tests with
```
cabal build joy && cabal exec joy 
```

Or use the repl, see the [detailed README](exercises/hsmoney/README.md)
for instructions.

### You have Docker

Pull the image we have prepared and run it, using the following
commands:

```
docker pull mostalive/joyofproperties2015
docker run -d -p 2022:2022 --name joy
```

This starts a Docker container on localhost. For each session a new
password for the dockerx and root users are generated. Check it out:

```
docker logs joy
```

Copy-paste the password and login:

```
ssh -AX -p 2022 dockerx@localhost
```

`-A` forwards your ssh-agent credentials (if any), so you can fork the
repository on github and push when done. 
`-X` is for GUI lovers. It will allow you to run one of the editors with a graphical front-end
on your desktop. We have Emacs, Gedit, Sublime Text, and Atom installed.
On a Mac you will find that Sublime Text and Atom redraw their screen on
every keypress and are therefore slow. 

For command-line veterans, you can use Vim or log into ssh without `-X`
to get Emacs in terminal mode. There is `tmux` to get multiple
shell windows.

If you are on Windows and have Docker, but no proper SSH client, you might opt for a full desktop by
installing [x2go-client](http://wiki.x2go.org/doku.php/download:start) .
This is cool, but a bit more work.

### Running it in the cloud

#### Javascript 

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

#### Haskell - FPComplete IDE

Create an account on `fpcomplete.com`, log in and open this link:

https://www.fpcomplete.com/ide?git=https://github.com/qwaneu/property-based-tutorial.git

to open this git repository in an fp-complete IDE workspace. 

### If all else fails

Pair up with someone. We recommend this anyway for a better learning
experience.

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


