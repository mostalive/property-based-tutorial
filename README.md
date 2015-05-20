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
cabal install --only-dependencies --enable-tests
```

Run the tests with
```
cabal -j test
```

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

Cloud9 provides a development environment in the cloud: `https://c9.io/`

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

#### Haskell - FPComplete

`https://www.fpcomplete.com`

### If all else fails

Pair up with someone. We recommend this anyway for a better learning
experience.

