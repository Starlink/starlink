#!/bin/csh
unsetenv ICL_TASK_NAME
unsetenv NDF_FORMATS_OUT
unsetenv NDF_FORMATS_IN
$POLPACK_DIR/polimpx in=$1 < /dev/null >& /dev/null
