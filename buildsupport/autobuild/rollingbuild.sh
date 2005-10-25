#!/bin/sh

NAME="Test-AutoBuild"

# Exit immediately if command fails
set -e

# Print command executed to stdout
set -v

export PERL5LIB=`perl -e 'use Config; my $dir = $Config{sitelib}; $dir =~ s|/usr|$ENV{AUTO_BUILD_ROOT}|; print $dir'`

# Make things clean.

[ -f Makefile ] && make -k realclean ||:
rm -rf MANIFEST blib

# Make makefiles.

perl Makefile.PL PREFIX=$AUTO_BUILD_ROOT
make manifest
echo $NAME.spec >> MANIFEST

# Build the RPM.
make
make test

make INSTALLMAN3DIR=$AUTO_BUILD_ROOT/share/man/man3 install

rm -f $NAME-*.tar.gz
make dist

if [ -x /usr/bin/rpmbuild ]; then
  rpmbuild -ta --clean $NAME-*.tar.gz
fi

if [ -x /usr/bin/fakeroot ]; then
  fakeroot debian/rules clean
  fakeroot debian/rules DESTDIR=$HOME/packages/debian binary
fi
