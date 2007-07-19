#! /bin/sh

# Run this to generate all the auto-generated files needed by the GNU
# configure program

libtoolize --automake
aclocal
automake --add-missing --gnu
autoconf
echo "Now use ./configure --enable-maintainer-mode"
