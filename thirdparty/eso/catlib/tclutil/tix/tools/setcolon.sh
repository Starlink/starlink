#!/bin/sh
#
# setcolon.sh --
#
#
# This program replaces all occurrences of :: in a file to :. It can
# be used to convert Tix mega widget classes to use the new convention
# of the single colon qualifier in method names.
#
# *Use with caution*. You may not want to replace all double colons in
# your program with single colons. The following is the entire
# program. Please examine carefully before execution.
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


if test "$*" = ""; then
    echo Usage: $0 file [files ...]
    echo
    cat $0
    exit
fi

for i in $*; 
do
    echo "modifying $i";
    sed -e 's|::|:|' $i > $i.tmp;
    mv $i.tmp $i;
done;
