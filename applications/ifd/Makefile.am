## Process this file with automake to produce Makefile.in

## Scripts which end up in the binary installation directory.

dist_bin_SCRIPTS = ifd_front.tcl

nodist_bin_SCRIPTS = ifd2iraf ifd2star ifd2irafifl ifl2ifd ifd_irafhlpgen

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

EXTRA_DIST = ifd2iraf.in ifd2star.in ifd2irafifl.in ifl2ifd.in \
             ifd_irafhlpgen.in \
             ifd_front.tcl tester.sh testc.ifd test.ifl testc.ifl testc.cl

## Test script.

TESTS = tester.sh

dist_starnews_DATA = ifd.news
