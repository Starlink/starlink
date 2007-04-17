#!/bin/sh
# These commands need to be run as root to build the skycat RPM

dir=/usr/src/redhat
test -d $dir || dir=/usr/src/packages

if test ! -d $dir ; then
    echo "Can't build RPM since there is no /usr/src/redhat or /usr/src/packages dir."
    exit 1
fi

if test ! -f skycat.spec ; then
    echo "skycat.spec does not exist: Please run configure to generate."
    exit 1
fi

version=`cat ../VERSION`
src=../release/$version.tar.gz
if test ! -f $src ; then
    echo "$src does not exist: Please run 'make release' to generate."
    exit 1
fi

(cp $src $dir/SOURCES/ && cp skycat.spec $dir/SPECS/ && cd $dir/SPECS && rpmbuild -ba skycat.spec)
