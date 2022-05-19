#!/bin/sh

#  Wrapper routine for CCDIMP to unset ICL_TASK_NAME variable  so we
#  can run from cl (and presumably Tcl).
unset ICL_TASK_NAME
$CCDPACK_DIR/ccdimp in=$1 table=$CCDPACK_DIR/import.table
exit
# $Id$
