#!/bin/csh
unsetenv ICL_TASK_NAME
unsetenv NDF_FORMATS_IN
unsetenv NDF_FORMATS_OUT
$POLPACK_DIR/polexp in=$1 quiet=y < /dev/null >& /dev/null
