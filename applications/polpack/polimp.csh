#!/bin/csh
unsetenv ICL_TASK_NAME
unsetenv NDF_FORMATS_OUT
unsetenv NDF_FORMATS_IN
$POLPACK_DIR/polimp in=$1 quiet=y < /dev/null >& /dev/null
