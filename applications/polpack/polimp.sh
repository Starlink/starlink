#!/bin/sh
unset ICL_TASK_NAME
unset NDF_FORMATS_OUT
unset NDF_FORMATS_IN
$POLPACK_DIR/polimpx in=$1 < /dev/null > /dev/null 2>&1
