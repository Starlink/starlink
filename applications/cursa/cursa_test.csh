#!/bin/csh -f
#+
#  Name:
#     cursa_test.csh
#
#  Purpose:
#     Test the CURSA applications.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     source cursa_test.csh
#
#  Description:
#     Test that the CURSA applications are installed correctly.
#
#  Authors:
#     ACD: A C Davenhall (Edinburgh)
#     BLY: M.J.Bly (Starlink, RAL)
#
#  History:
#     15/10/95 (ACD):
#        Original version.
#     24/10/95 (BLY):
#        Modified for including with CURSA distribution.
#     20/12/96 (ACD):
#        Modified for CURSA version 2.1 (added catselect).
#     1/6/01  (ACD):
#        Fixed bug in the way CURSA is set up and the files obtained.
#-
#

echo " "
echo "Set up CURSA..."

INSTALL_BIN/cursa.csh

echo " "
echo "Copy test data and scripts..."

mkdir ./cursatest
cd ./cursatest

cp INSTALL_BIN/5828.gsc .
cp INSTALL_BIN/catselect.script .
cp INSTALL_BIN/catpair.script .
cp INSTALL_BIN/catview.script .
cp INSTALL_BIN/targets.TXT .

echo " "
echo "catgscin..."
INSTALL_BIN/catgscin 5828.gsc

echo " "
echo "catselect..."
INSTALL_BIN/catselect  < catselect.script

echo " "
echo "catpair..."
INSTALL_BIN/catpair  < catpair.script

echo " "
echo "catsort..."
INSTALL_BIN/catsort join5828 sort5828 MAG ascending

echo " "
echo "catcopy..."
INSTALL_BIN/catcopy sort5828 copy5828

echo " "
echo "catheader..."
INSTALL_BIN/catheader copy5828 N

echo " "
echo "catview..."
INSTALL_BIN/catview < catview.script 

#
# Tidy up.

rm -f gsc5828.FIT 
rm -f select5828.FIT 
rm -f join5828.FIT 
rm -f sort5828.FIT 
rm -f copy5828.FIT

rm -f 5828.gsc 
rm -f catselect.script
rm -f catpair.script 
rm -f catview.script 
rm -f targets.TXT

cd ..
rm -r ./cursatest

#
exit 0
#.
