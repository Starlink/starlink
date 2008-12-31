#!/bin/csh -f
#+
#  Name:
#     cat-examples_test.csh
#
#  Purpose:
#     Test the CAT example applications.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     source cat-examples_test.csh
#
#  Description:
#     Test that the CAT example applications are installed correctly.
#
#  Authors:
#     ACD: A C Davenhall (Edinburgh)
#
#  History:
#     15/10/95 (ACD): Original version.
#     12/11/97 (ACD): Converted from the original CATAPP package to
#        CAT examples.
#-
#

echo " "
echo "Set up CAT examples..."

source /star/bin/examples/cat/cat-examples.csh

echo " "
echo "EXAMPLE_WRITE - create a catalogue..."
EXAMPLE_WRITE  test_cat

echo " "
echo "EXAMPLE_READ - read back the catalogue..."
EXAMPLE_READ   test_cat

#
# Tidy up.

rm test_cat.FIT
