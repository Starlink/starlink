#!/bin/sh
#
# E.S.O. - VLT project/ESO Archive
# $Id: skycatgaia.sh,v 1.2 1999/02/04 22:12:23 abrighto Exp $ 
#
# This script can be used to start Skycat with the GAIA plugin
# without having to set any environment variables.
#
# usage:  skycatgaia ?options?
#
# Where the options are passed on to skycat.
#
# This script must be installed in the gaia_plugin directory.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 3 Feb 99   created

# skycat binary to use
skycat=skycat-"`uname -s`-`uname -r`"

# determine the location of this script and use that to determine the
# location of the gaia_plugin directory.
if [ -f "$0" ]; then
    file=$0
else
    for i in `echo $PATH | tr ':' ' '`
    do
	if [ -f $i/$0 ] ; then
	    file=$i/$0
	    break
	fi
	done
fi

SKYCAT_PLUGIN=`dirname $file`; export SKYCAT_PLUGIN

SHLIB_PATH="${SHLIB_PATH}:${SKYCAT_PLUGIN}"; export SHLIB_PATH
LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${SKYCAT_PLUGIN}"; export LD_LIBRARY_PATH

exec $skycat "$@"
