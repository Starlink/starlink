#!/bin/sh

#+
#  Name:
#     size_test.sh

#  Purpose:
#     Test for SCB to see if it handles C files with long comments.

#  Description:
#     If you build the tagging routines using flex in its default
#     configuration, it fails on some files with very long tokens
#     (usually comments).  Good examples are to be found in the 
#     AST package.  This script tries the C tagger against some of
#     these to see whether it chokes or not.
#
#     The build machinery which persuades FLEX to build a scanner 
#     which passes this test involves using a custom flex.skl file.
#     This may not be very portable.

#  Authors:
#     MBT: Mark Taylor (Starlink)

#  History:
#     28-FEB-2005 (MBT):
#        Original version.
#-

#  Test file.  This is a copy of a file from AST which is known to
#  cause long-token problems.
      ./ctag test_frame.c >/dev/null

