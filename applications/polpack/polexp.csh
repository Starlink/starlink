#!/bin/csh
unsetenv ICL_TASK_NAME
$POLPACK_DIR/polexp in=$1 quiet=y < /dev/null >& /dev/null
