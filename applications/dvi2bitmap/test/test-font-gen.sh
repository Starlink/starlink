#! /bin/sh -
#
# Test script, to check that dvi2bitmap is working, and to tell the user
# how to set DVI2BITMAP_PK_PATH.  Should be invoked from the Makefile.
#
# $Id$

if [ $# != 2 ]; then
    echo "Usage: $0 path-to-dvi2bitmap test-DVI-file"
    exit 1
fi

d2bpath=$1
infile=$2

KPW=`which kpsewhich`

if [ -z "$KPW" ]; then
    echo "Couldn't find kpsewhich!"
    exit 1
fi

# Force the output filename, and set what file we expect to be generated
opfmt='-o test-output-%d'
defaultftype=`$d2bpath -Qt | awk '/^Qt/{print $2}'`
opfname="test-output-1.$defaultftype"

rm -f $opfname

# Find out what was enabled in dvi2bitmap,
# by parsing the output with the -V option
eval `$d2bpath -V | sed -n '/^ENABLE/s/$/=1/p'`

if [ -z "$ENABLE_FONT_GEN" ]; then
    echo "Font generation disabled"
else
    echo "Font generation enabled"
fi
if [ -z "$ENABLE_KPATHSEA" ]; then
    echo "Kpathsea disabled"
else
    echo "Kpathsea enabled"
fi
echo

if [ -n "$ENABLE_KPATHSEA" ]; then

    if [ -n "$ENABLE_FONT_GEN" ]; then
	echo "You have enabled both the kpathsea library and"
	echo "font generation.  I will attempt to convert the test file."
	echo "This should simply Work."
    else
	echo "You have enabled the kpathsea library, but disabled"
	echo "font-generation (you presumably know what you're doing)."
	echo "I will not, therefore, attempt to create any fonts,"
	echo "but will simply try to convert the test file...."
    fi
    $d2bpath $opfmt $infile

else
	
    # no kpathsea
    if [ -n "$ENABLE_FONT_GEN" ]; then
	echo
	echo "You have enabled font-generation.  Either you have disabled"
	echo "use of the kpathsea library, or else it is not available."
	echo
	echo "I will try to generate the fonts required for the test file...."
	$d2bpath -Qg -n $infile 2>/dev/null | \
	    sed -n '/^Qg/s/^Qg *//p' | \
	    sh

	fontname=`$d2bpath -QF -n $infile 2>/dev/null | \
	    awk '/^Qf/{printf "%s.%spk",$2,$3}'`
	echo
	echo "Looking for font $fontname..."
	fontnamepath=`$KPW $fontname`
	if [ -n "$fontnamepath" ]; then
	    echo
	    echo "Found font $fontname in"
	    echo "  " $fontnamepath
	    echo "Good."
	else
	    echo "I thought I'd generated font $fontname,"
	    echo "but I (or rather kpsewhich) can't find it anywhere."
	    echo "This is most puzzling.  Did the font-generation above"
	    echo "work?  If it did work, why doesn't kpsewhich find the"
	    echo "generated fonts?  There may be some problem with your"
	    echo "TeX setup."
	    exit 1
	fi

	d2bpkpath=`echo $fontnamepath | sed 's+/[^/]*$++'`
	echo
	echo "In future, you need to set the environment variable"
	echo "DVI2BITMAP_PK_PATH to the directory"
	echo "  " $d2bpkpath
	echo "for dvi2bitmap to work.  I'll make things work now,"
	echo "by using dvi2bitmap's -fp option, which has the same effect."

	echo
	echo "Trying to convert the test file...."
	$d2bpath $opfmt -fp $d2bpkpath $infile
	
    else

	# No kpathsea and no font generation
	tfile="tmp-$$"
	$d2bpath -Qf -n $infile 2>/dev/null | grep '^Qf' > $tfile
	echo
	echo "Both use of the kpathsea library, and font-generation"
	echo "are disabled."
	if [ -s $tfile ]; then
	    echo "For dvi2bitmap to work, you need to (somehow)"
	    echo "generate the following fonts:"
	    awk '{printf "%s.%spk", $2, $3}' $tfile
	    echo "Then point dvi2bitmap to the directory containing"
	    echo "those fonts, using either the environment variable"
	    echo "DVI2BITMAP_PK_PATH, or the -fp option."
	    rm -f $tfile
	    exit 1
	else
	    rm -f $tfile
	    echo "However, all the required fonts appear to be available."
	    echo "So I'll try converting the test file...."
	    $d2bpath $opfmt $infile
	fi

    fi

fi

if [ -f $opfname ]; then
    echo
    echo "I've found file $opfname!"
    echo "dvi2bitmap works!"
else
    echo
    echo "I was expecting to find file $opfname, but couldn't."
    echo "Something is amiss"
    exit 1
fi

exit 0
