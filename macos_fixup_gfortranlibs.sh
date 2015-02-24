#!/usr/bin/env bash

# The four gfortran libraries that need to be shipped.

# /usr/local/lib/libgfortran.3.dylib
# /usr/local/lib/libquadmath.0.dylib
# /usr/local/lib/libstdc++.6.dylib
# /usr/local/lib/libgcc_s.1.dylib

# Give one of these as the first argument to this script and it will
# be copied in and fixed up.

# Note that the order you do these in matters, as they reference each
# other -- do so in the order above I think.

libpath="$1"

# So 1. copy these files into $STARLINK_DIR/lib
cp $libpath $STARLINK_DIR/lib/


# 2. fix up their ids.
bname=$(basename $libpath)
newpath=$STARLINK_DIR/lib/$bname
echo "chmod u+w $newpath"
chmod u+w $newpath
install_name_tool -id "$newpath" "$newpath"


# 3.Find all references to these files and fix them with the
# appropriate $STARLINK_DIR/lib path
for i in $(find $STARLINK_DIR/lib -name '*.dylib' -type f); do
    install_name_tool -change "$libpath" "$newpath" "$i";
done

for i in $(find $STARLINK_DIR/bin -type f -perm +111 -not -name '*.csh' \
    -not -name '*.sh' -not -name '*_link_adam' -not -name '*.icl' \
    -not -name '*.py' -not -name '*.pl' -not -name '*.tcl' \
    -not -name '*.tab' -not -name '*.cfg' -not -name '*.ids' \
    -not -name '*.def' -not -name '*.class'); do
    install_name_tool -change "$libpath" "$newpath" "$i";
done


for i in $(find $STARLINK_DIR/Perl -name '*.bundle' -type f); do
    install_name_tool -change "$libpath" "$newpath" "$i";
done


