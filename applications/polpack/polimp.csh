#!/bin/csh
unsetenv ICL_TASK_NAME
$POLPACK_DIR/polimp in=$1 quiet=y < /dev/null >& /dev/null
