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

## Arrange to edit the installation binary directory into script files
## which need it.  This cannot be done by autoconf because it needs to
## be changable at make time - see the autoconf manual section on 
## Installation Directory Variables.
## Since these ones haven't been modified by autoconf, we take the 
## opportunity to do any non-make-sensitive substitutions here as well.

edit = sed -e 's%@bindir\@%$(bindir)%g' \
           -e 's%@STARTCLSH\@%@TCLSH@%g'

ifd2iraf: Makefile $(srcdir)/ifd2iraf.in
	rm -f ifd2iraf ifd2iraf.tmp
	$(edit) $(srcdir)/ifd2iraf.in >ifd2iraf.tmp
	chmod +x ifd2iraf.tmp
	mv ifd2iraf.tmp ifd2iraf

ifd2star: Makefile $(srcdir)/ifd2star.in
	rm -f ifd2star ifd2star.tmp
	$(edit) $(srcdir)/ifd2star.in >ifd2star.tmp
	chmod +x ifd2star.tmp
	mv ifd2star.tmp ifd2star

CLEANFILES = ifd2star ifd2iraf

dist_starnews_DATA = ifd.news


