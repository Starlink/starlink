#! /bin/sh -
#+
#  Name:
#    sgml_set.sh
#
#  Purpose:
#    Set up the Starlink SGML system from the shell.
#
#  Type of module:
#    Shell script
#
#  Invocation:
#    . sgml_system.sh
#    (since the script sets up environment variables, it cannot simply
#    be executed as a script)
#
#  Authors:
#    NG: Norman Gray (Starlink, Glasgow)
#
#  History:
#    16-JUN-1999 (NG):
#      Original version
#    30-SEP-1999 (NG):
#      Remove sgml2* aliases, and define the PATH
#
# $Id$
#-

# Don't bother making the thing particularly sophisticated.  We don't
# check if environment variables are already defined.

STARLINK_SGML_DIR=((STARLINK_SGML_DIR))
export STARLINK_SGML_DIR

SGML_CATALOG_FILES=./CATALOG:$STARLINK_SGML_DIR/dtd/CATALOG:$SGML_CATALOG_FILES
export SGML_CATALOG_FILES

PATH=((SGML_BINDIR)):$PATH



echo "Set up ((PACK_NAME)), release ((PKG_VERS))"
