#!/bin/sh
unset ICL_TASK_NAME
unset NDF_FORMATS_IN
unset NDF_FORMATS_OUT
$POLPACK_DIR/polexpx in=$1 < /dev/null > /dev/null 2>&1
