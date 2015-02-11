#! /bin/csh -f
#+
#  Name:
#    sgml_set.csh
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
#    4-JUN-2000 (NG):
#      Add test of whether SGML_CATALOG_FILES is already defined,
#      and append if so.
#
# $Id$
#-

# No need to make the thing particularly sophisticated.

setenv STARLINK_SGML_DIR ((STARLINK_SGML_DIR))

if ($?SGML_CATALOG_FILES) then
    setenv SGML_CATALOG_FILES ./CATALOG:$STARLINK_SGML_DIR/dtd/CATALOG:$SGML_CATALOG_FILES
else
    setenv SGML_CATALOG_FILES ./CATALOG:$STARLINK_SGML_DIR/dtd/CATALOG
endif

setenv PATH ((SGML_BINDIR)):$PATH


echo "Set up ((PACK_NAME)), release ((PKG_VERS))"
