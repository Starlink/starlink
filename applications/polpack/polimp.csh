#!/bin/csh
unsetenv ICL_TASK_NAME
$POLPACK_DIR/polimp in=$1 quiet=y table=$POLPACK_DIR/polimp.tab < /dev/null >& /dev/null
