#! /bin/sh -
#
# Test script, to check that dvi2bitmap is working, and to tell the user
# how to set DVI2BITMAP_PK_PATH.  Should be invoked from the Makefile.
#
# This script does make certain `normal' assumptions about the
# behaviour of mktexpk (or MakeTeXPK): it assumes that if it finds
# where cmr10 (or whatever font(s) is/are included in the test DVI
# file) has been placed, by asking kpsewhich to report its location,
# then this is a reasonable directory to have as the value of
# DVI2BITMAP_PK_PATH.
#
# $Id$

if [ $# -lt 2 ]; then
    echo "Usage: $0 path-to-dvi2bitmap test-DVI-file [output-file-root]"
    exit 1
fi

d2bpath=$1
infile=$2
if test -n "$3"; then
    opfroot=$3
else
    opfroot=test-output
fi

# Check that kpsewhich is in the path.  Can't reliably use `which' here,
# since it's sometimes a stoopid script.
for d in `echo $PATH | sed 's/:/ /g'`; do
    if test -f $d/kpsewhich -a -z "$KPW"; then
	KPW=$d/kpsewhich
    fi
done

if [ -z "$KPW" ]; then
    echo "Couldn't find kpsewhich in $PATH"
    exit 1
fi

# Force the output filename, and set what file we expect to be generated
opfmt="-o $opfroot-%d"
defaultftype=`$d2bpath -Qt -nn | awk '/^Qt/{print $2}'`
opfname="$opfroot-1.$defaultftype"
rm -f $opfname

preline='vvvvvvvvvvvvvvvvvvvv'
postline='^^^^^^^^^^^^^^^^^^^^'

# Find out what was enabled in dvi2bitmap,
# by parsing the output with the -V option
#eval `$d2bpath -V | sed -n '/^ENABLE/s/  */=/p'`
eval `$d2bpath -V | sed -n 's/^\([A-Z_][A-Z_]*\)  *\(.*\)$/D2B_\1=\"\2\"/p'`

if [ -n "$D2B_FONT_GEN_TEMPLATE" ]; then
    echo "Font generation ... enabled"
else
    echo "Font generation ... disabled"
fi
if [ "$D2B_ENABLE_KPATHSEA" = yes ]; then
    echo "Kpathsea .......... enabled"
else
    echo "Kpathsea .......... disabled"
fi
echo

if [ "$D2B_ENABLE_KPATHSEA" = yes ]; then

    echo
    if [ -n "$D2B_FONT_GEN_TEMPLATE" ]; then
	echo "You have enabled both the kpathsea library and"
	echo "font generation.  I will attempt to process the test file."
	echo "This should simply Work, and you don't have to do anything"
	echo "to tell dvi2bitmap where its fonts are."
    else
	echo "You have enabled the kpathsea library, but disabled"
	echo "font-generation (that's a non-standard configuration,"
	echo "but I'll assume you know what you're doing)."
	echo "I will not, therefore, attempt to create any fonts,"
	echo "but will simply try to convert the test file...."
    fi
    echo $preline
    echo "% $d2bpath $opfmt $infile"
    $d2bpath $opfmt $infile
    echo $postline
    d2bstatus=$?

else
	
    # no kpathsea
    if [ -n "$D2B_FONT_GEN_TEMPLATE" ]; then
	echo
	echo "You have enabled font-generation.  Either you have disabled"
	echo "use of the kpathsea library, or else it is not available."
	echo
	echo "You therefore MUST set the environment variable"
	echo "DVI2BITMAP_PK_PATH (or use the -fp option), if dvi2bitmap"
	echo "is to find the fonts it asks the system to generate."
	echo
	echo "I will now try to generate the fonts required for the test file."
	echo "This might be redundant, but it won't be wrong."
	echo $preline
	echo "% $d2bpath -Qg -n $infile -q | sed -n '/^Qg/s/^Qg *//p' | sh"
	$d2bpath -Qg -n $infile -q | \
	    sed -n '/^Qg/s/^Qg *//p' | \
	    sh
	echo $postline

	# Get just the first fontname, in case there's more than one
	fontname=`$d2bpath -QF -n $infile -q | \
	    awk '/^Q[fF]/{printf "%s.%spk",$2,$3; exit}'`
	echo
	echo "Looking for font $fontname..."
	fontnamepath=`$KPW pk $fontname`
	if [ -n "$fontnamepath" ]; then
	    echo
	    echo "Found font $fontname in"
	    echo "  " $fontnamepath
	    echo "Good."
	else
	    echo "I thought I'd generated font $fontname,"
	    echo "but I (or rather kpsewhich) can't find it anywhere."
	    echo "This is most puzzling."
	    echo "Did the font-generation above work?"
	    echo "If it did work, why doesn't kpsewhich find the generated fonts?"
	    echo "There may be some problem with your TeX setup."
	    exit 1
	fi

	d2bpkpath=`echo $fontnamepath | sed 's+/[^/]*$++'`
	echo
	echo "In future, you need to set the environment variable"
	echo "DVI2BITMAP_PK_PATH to "
	echo "  " $d2bpkpath/%f.%dpk
	echo "for dvi2bitmap to work.  I'll make things work now,"
	echo "by using dvi2bitmap's -fp option, which has the same effect."

	echo
	echo "Trying to convert the test file...."
	echo $preline
	echo "% $d2bpath $opfmt -fp $d2bpkpath/%f.%dpk $infile"
	$d2bpath $opfmt -fp $d2bpkpath/%f.%dpk $infile
	d2bstatus=$?
	echo $postline
	
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
	    awk '{printf "%s.%spk\n", $2, $3}' $tfile
	    echo "Then point dvi2bitmap to the directory containing"
	    echo "those fonts, using either the environment variable"
	    echo "DVI2BITMAP_PK_PATH, or the -fp option."
	    rm -f $tfile
	    exit 1
	else
	    rm -f $tfile
	    echo "However, all the required fonts appear to be available."
	    echo "So I'll try converting the test file...."
	    echo $preline
	    echo "% $d2bpath $opfmt $infile"
	    $d2bpath $opfmt $infile
	    d2bstatus=$?
	    echo $postline
	fi

    fi

fi

if [ -f $opfname ]; then
    echo
    echo "I've found file $opfname!"
    if [ $d2bstatus -ne 0 ]; then
	echo "but dvi2bitmap returned an error status"
    else
	echo "dvi2bitmap works!"
    fi
else
    echo
    echo "I was expecting to find file $opfname, but couldn't."
    echo "Something is amiss"

    texmfcnf=`type kpsewhich >/dev/null 2>&1 && kpsewhich cnf texmf.cnf 2>/dev/null`
    if [ -n "$texmfcnf" ]; then
	# kpsewhich exists, and found texmf.cnf file.
	# The following test isn't bulletproof, but should at least
	# give the user a pointer to the right place in the manual.
	if grep 'FONTS.*AUTO' $texmfcnf >/dev/null; then
	    echo ""
	    echo "Aha!"
	    echo "I think that your texmf.cnf file ($texmfcnf) may be broken."
	    echo "You may have to use the --enable-fake-progname"
	    echo "configuration option, as described in the manual."
	fi
    fi
    echo ""
    echo "See the section on 'Not finding fonts' in the manual."

fi

echo

exit $d2bstatus
