#!/bin/csh -f

setenv CC cc
setenv CXX CC

set dir = `(cd ../..; dirs)`
configure -prefix $dir/install --with-cc

