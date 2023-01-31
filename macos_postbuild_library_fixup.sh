#!/usr/bin/env bash

# This script attempts to fix up the .dylibs and binaries built on a
# mac so that all libraries have a full path, preventing the need to
# set DYLD_LIBRARY_PATH before running the software. It uses
# install_name_tool to do this, and otool -L to inspect the libraries
# and binaries.

# It requires the environ variable $STARLINK_DIR to be set so it knows
# where to find the installed libraries and binaries.

# It also uses python...

# 17th Feb: not extensively tested, use at own risk.

# Define a function that returns the relative path between two files.
function relpath(){ python3 -c "import os.path; print(os.path.relpath('$1','${2:-$PWD}'))" ; }




# Define a function that takes the name of a library or binary, runs
# otool -L and fixes up the links to:
#
# a) any librarys without paths that can be found in the $STARLINK_DIR/lib, and
# b)and librarys that are already known to be in $STARLINK_DIR
#
# by replacing replacing the $STARLINK_DIR with an @rpath

function fixup_starlink_dylib_links()
{
    # run otool -L on the library or binary, go through each line of
    # results (but not the initial line giving the name of the
    # library)
    #echo $1
    unset libline
    otool -L $1 |grep 'version'| grep -v 'is not an object file' |while read libline; do
	unset lib
	unset truepath

	# split $libline at spaces; first result is the 'path' of the
	# library
	a=($libline)
	lib=${a[0]}
	unset a
	#echo "libline and lib $libline  $lib"
	# Check if it is a full path.
	if [[ -e $lib ]]; then
	    truepath=$lib
	else
	    # see if we can find the file in $STARLINK_DIR/lib
	    paths=$(find $STARLINK_DIR/lib -name "$lib")
	    truepath=${paths[0]}

	    # Check if find returned anything; move onto next line of find if not
	    if [ "$truepath" = "" ]; then
		echo "WARNING: could not find $lib in $STARLINK_DIR/lib" 1>&2
		unset truepath
		unset lib
		continue
	    fi
	fi
        # Be extra cautious and check the path is correct.
	if [[ -e $truepath ]]; then

	    # Now fix up the library with the true path
	    # 1. differentiate between the id and the loaded paths.
	    unset baselibname
	    unset base
	    baselibname=$(basename $truepath)
	    base=$(basename $1)

	    if [[ "$baselibname" = "$base" ]]; then
		# this is the library id; so don't do anything.
		:

	    else
		# Ensure that $STARLINK_DIR occurs at front of path
		if [[ $truepath =~ $STARLINK_DIR.* ]]; then

		    # Replace the $STARLINK_DIR section with @rpath
		    truepath="${truepath/$STARLINK_DIR/@rpath}"
		fi

		# Don't bother updating if $lib and $truepath are the same.
		if [[ $truepath != $lib ]]; then
		    install_name_tool -change "$lib" "$truepath" "$1"
		fi
	    fi
	else
	    echo "WARNING: did not find $lib at $truepath" 1>&2
	fi
    done
}

# Define a function that replaces the $STARLINK_DIR part of the *id* of
# a shared library (.dylib)  with @rpath.
function replace_dylibid_starlink_rpath()
{
    # Run otool -D on the library, get the second line
    otooloutput=$(otool -D $1)
    output=(${otooloutput//\\n/ })
    libname=${output[1]}

    # Check if file is full path and if it contains STARLINK_DIR
    if [[ -e $libname && $libname =~ $STARLINK_DIR.* ]]; then

	# replace $STARLINK_DIR with @rpath in libname and set id to that
	newid="${libname/$STARLINK_DIR/@rpath}"
	install_name_tool -id "$newid" "$1"

    # Otherwise look for the file within in STARLINK_DIR/lib
    elif [[ -n $(find "$STARLINK_DIR/lib" -type f -name "$libname") ]]; then

	# get path of file
	paths=$(find $STARLINK_DIR/lib -type f -name "$libname")
	truepath=${paths[0]}

	# Replace $STARLINK_DIR with @rpath
	newid="${truepath/$STARLINK_DIR/@rpath}"
	install_name_tool -id "$newid" "$1"
    fi
}

function add_rpath_starlink()
{
    # Get the relative location of the $STARLINK_DIR from the current binary/dylib.
    relpath=$(relpath $STARLINK_DIR $(dirname $1))

    # set the rpath
    install_name_tool -add_rpath "@loader_path/$relpath/" "$1"
}


# 1. Go through each .dylib or .so living under lib/ and set all the loaded
# libraries that live within starlink to be relative paths.
# Then add the appropriate rpath to the library
echo "Fixing up .dylibs and .so under $STARLINK_DIR/"
echo
echo
for i in $(find $STARLINK_DIR/ -name '*.dylib' -or -name '*.so' -type f); do
     # echo $i
     fixup_starlink_dylib_links $i
     add_rpath_starlink $i
done

# 2. Fix up all .bundles living under Perl/.
echo "Fixing up .bundles under $STARLINK_DIR/Perl"
echo
echo
for i in $(find $STARLINK_DIR/Perl -name '*.bundle' -type f); do
    fixup_starlink_dylib_links $i
    add_rpath_starlink $i
done



# 2. Do the same thing for each binary living under bin/
echo "Fixing up binaries under $STARLINK_DIR/bin"
echo
echo
for i in $(find $STARLINK_DIR/bin -type f -perm +111); do

    # Check if a Mach-O type file.
    if file $i | grep -q Mach-O; then
	fixup_starlink_dylib_links $i
	add_rpath_starlink $i
    fi

done

# We now have all Starlink build libraries being set to their full path.
# We now want to replace all of the $STARLINK_DIR.. paths in the library ids with an @rpath

for i in $(find $STARLINK_DIR/lib -type f -name '*.dylib'); do
    replace_dylibid_starlink_rpath $i
done
