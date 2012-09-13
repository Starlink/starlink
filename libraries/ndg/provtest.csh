#!/bin/tcsh

#  This script compiles the provenance tester and runs it first on a file
#  that uses the old ASCII scheme for storing provenance in an HDS structure.
#  It then converts the NDF to use the new binary storage scheme and does
#  the test again.
#
#  Note this script requires a working KAPPA installation and so cannot
#  be included in the NDG makefile check target.

g95 -g -fno-second-underscore -o provtest ndg_provtest.f -L/star/lib \
                                 -I/star/include `ndg_link`

echo
echo "Testing old ASCII storage scheme..."
./provtest

cp provtest.sdf oldprovtest.sdf
$KAPPA_DIR/provmod provtest

echo
echo "Testing new binary storage scheme..."
./provtest

mv -f oldprovtest.sdf provtest.sdf

