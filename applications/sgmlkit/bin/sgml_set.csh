#! /bin/csh -f
#+
#  Name:
#    sgml_system.csh
#
#  Purpose:
#    Set up the Starlink SGML system from the shell.
#
#  Type of module:
#    C-Shell script
#
#  Invocation:
#    source sgml_system.sh
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
#      Remove sgml2* aliases and define PATH
#
# $Id$
#-

# Don't bother making the thing particularly sophisticated.  We don't
# check if environment variables are already defined.

setenv STARLINK_SGML_DIR ((STARLINK_SGML_DIR))

setenv SGML_CATALOG_FILES ./CATALOG:$STARLINK_SGML_DIR/dtd/CATALOG:$STARLINK_SGML_DIR/documents/CATALOG

setenv PATH ((SGML_BINDIR)):$PATH


echo "Set up ((PACK_NAME)), release ((PKG_VERS))"
