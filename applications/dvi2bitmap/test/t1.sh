#! /bin/sh -
if test $# -eq 0; then
    echo SHELL=$SHELL
else
    while test $# -gt 0; do
        eval RET="${RET}$1=\${$1}!"
	shift
    done
    echo $RET
fi
echo "Don't see this"

