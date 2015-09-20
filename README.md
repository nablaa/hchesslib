# hchesslib

[![Build Status](https://travis-ci.org/nablaa/hchesslib.png?branch=master)](https://travis-ci.org/nablaa/hchesslib)

hchesslib is a simple chess library for Haskell.

## Building

Building the library inside cabal sandbox:

    cabal sandbox init
    cabal install --only-dependencies --enable-tests
    cabal build

## Running tests

Running unit tests after the library has been built:

    cabal test
