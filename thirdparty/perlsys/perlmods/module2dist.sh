#!/usr/bin/env bash

# Looks at the cpan.deps file and generates output that converts the generic
# module name to an explicit CPAN distribution. This is important for tagged
# releases where we want to ensure that we recreate our system using the
# same versions of modules as were used for the release rather than the
# versions currently on CPAN during the new build.

# Note that this script is not bullet proof because it does not list
# the versions of all the dependencies of these modules to ensure that
# those versions are also tracked.

# Requires cpanm and does not install anything
# It will read the cpan.deps related files and the OS-specific variants.
# It writes to explicit-cpan.deps files and OS-specific variants.

# Location of Perl and support libraries
perlroot=${STARLINK_DIR}/Perl/bin/

cpanm=${perlroot}/cpanm

# Make sure that CPANM is up to date
if [ -x $cpanm ]
then
    ${cpanm} --self-upgrade 1>&2
else
    curl -L http://cpanmin.us | $starperl - App::cpanminus
fi

# For OS-specific version files we need to look at them
# all regardless of the operating system we are actually using
for depsfile in cpan.deps cpan.deps.*
do
    echo Analyzing $depsfile
    fulldeps=explicit-${depsfile}
    if [ -e $fulldeps ]
    then
        rm -f $fulldeps
        touch $fulldeps
    fi
    for i in `cat $depsfile`
    do
        echo "  $i"
        ${cpanm} --info $i >> $fulldeps
        if (( $? ))
        then
            echo Failed to translate $i
            exit 1
        fi
    done
done
