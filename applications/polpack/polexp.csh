#!/bin/csh
unsetenv ICL_TASK_NAME
unsetenv NDF_FORMATS_IN
unsetenv NDF_FORMATS_OUT
$POLPACK_DIR/polexpx in=$1 < /dev/null >& /dev/null
