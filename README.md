# property-based-tutorial
workshop materials and experiments for property based testing tutorial
at joy of coding 2015. 

## Install
Nothing much useful here yet, just some experiments with property based
testing we develop in the open, to see which example is the most useful
for a tutorial.

The tutorial comes in a javascript (because virtually everyone has to
use it these days) and haskell (because that is where property-based
testing really gained traction, with QuickCheck) flavours.

## Installation options

### You have a NodeJs or a working Haskell installation on your machine

clone this repository `git clone
https://github.com/qwaneu/property-based-tutorial.git`

#### Javascript

```
cd exercises/jsmoney
npm ???

```

#### Haskell

```
cd exercises/hsmoney
cabal install --only-dependencies --enable-tests
```

Run tests with
```
cabal -j test
```

### You have docker

```
docker pull mostalive/joyofproperties2015
docker run -d -p 2022:2022 --name joy
```

This starts a docker container on localhost. For each session a new
password for the dockerx and root user is generated. Check it out:

```
docker logs joy
```

Copy-paste the password, and login.

```
ssh -AX -p 2022 dockerx@localhost
```

`-A` forwards your ssh-agent credentials (if any), so you can fork the
repository on github and push when done. 
`-X` is for Gui lovers. It will allow you to run one of the editors with a graphical front-end
on your desktop. We have emacs, gedit, sublime-text, and atom installed.
On a Mac you will find that sublime-text and atom redraw their screen on
every keypress, and are therefore slow. 

For command-line veterans, you can use vim, or log into ssh without `-X`
to get emacs in terminal mode. There is `tmux` to get multiple
shell windows.

If you are on windows and have docker, but no proper ssh client, you might op for a full desktop, by
installing [x2go-client](http://wiki.x2go.org/doku.php/download:start) .
This is cool, but a bit more work.

### You have a windoze machine and only a web browser

#### NodeJS - cloud9 ide?

#### Haskell - FPComplete
