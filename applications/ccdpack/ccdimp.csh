#!/bin/csh

#  Wrapper routine for CCDIMP to make sure that format conversion
#  isn't attempted when passed a data file. We should always get a
#  plain NDF. The ICL_TASK_NAME variable is unset so we can run from 
#  cl (and presumably Tcl).

unsetenv NDF_FORMATS_IN
unsetenv NDF_FORMATS_OUT
unsetenv ICL_TASK_NAME
$CCDPACK_DIR/ccdimp in=$1 table=$CCDPACK_DIR/import.table

exit
# $Id$
