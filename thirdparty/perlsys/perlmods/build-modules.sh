#!/usr/bin/env bash

# Installs cpanm or updates it
# Then goes through each perl-* directory and installs the modules with cpanm

# Location of Perl and support libraries
perlroot=${STARLINK_DIR}/Perl/bin/
export CFITSIO=${STARLINK_DIR}

starperl=${perlroot}/perl
cpanm=${perlroot}/cpanm

# Make sure that CPANM is up to date
if [ -x $cpanm ]
then
    ${cpanm} --self-upgrade
else
    curl -L http://cpanmin.us | $starperl - App::cpanminus
fi

# First do the CPAN dependencies required
# to run Starlink perl applications
# We include OS-specific entries
osdeps=cpan.deps.`uname`
if [ ! -e $osdeps ]
then
    osdeps=""
fi
for i in `cat ./cpan.deps $osdeps`
do
    ${cpanm} $i
    if (( $? ))
    then
        echo Failed to install $i
        exit 1
    fi
done

# Then do the local repositories. We have to be careful
# about the install order so get the order from an external
# file.

# CPANM rebuilds all of these each time it runs

for i in `cat ./perlmods-order.txt`
do
    if [ -d ./$i ]
    then
        ${cpanm} ./$i
        if (( $? ))
        then
            echo Failed to install $i
            exit 1
        fi
    fi
done

# Create any necessary symlinks from the Starlink bin directory.

PERLMOD_DIR=$PWD
pushd ${STARLINK_DIR}/bin
for i in $(< ${PERLMOD_DIR}/perlmods-symlinks.txt)
do
    if [ ! -e $i ]
    then
        echo Making symlink for ${i}.
        ln -s ../Perl/bin/$i
    else
        echo Symlink for $i does not need to be created, skipping.
    fi
done
popd
