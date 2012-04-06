#!/bin/sh

runhaskell Setup configure --prefix=. --user
runhaskell Setup build
runhaskell Setup install
