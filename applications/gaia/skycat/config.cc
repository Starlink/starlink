#!/bin/csh -f


set dir = $PWD

setenv CC cc
setenv CXX CC

configure -prefix $dir/../install --with-cc
make

