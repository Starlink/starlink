## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all

$(RECURSIVE_TARGETS):
	( cd cfitsio; $(MAKE) $@; )

## Library linking script.
dist_bin_SCRIPTS = cfitsio_link

EXAMPLE_SOURCES = cfitsio/utilities/cookbook.c cfitsio/utilities/cookbook.f

# Put examples in dist_pkgdata
dist_pkgdata_DATA = $(EXAMPLE_SOURCES) CFITSIO_CONDITIONS \
    cfitsio/ChangeLog \
    cfitsio/licenses/License.txt

# Extra documentation for stardocs (not converted to hypertext).
stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ mud162.tex mud167.tex mud166.tex

#  Clean recurses into cfitsio.
clean-local:
	(cd cfitsio; $(MAKE) clean )

distclean-local:
	(cd cfitsio; $(MAKE) distclean )

## Need all the source code in the cfitsio sub-directory. Latex docs
## don't seem to be added either, so must go here.
EXTRA_DIST = cfitsio mud162.tex mud167.tex mud166.tex

# Before distributing the subdirectory we need to clean it up (we get all
# the files). Also remove the CVS trace.
dist-hook:
	rm -rf `find $(distdir) -name CVS`
	(cd $(distdir)/cfitsio; make distclean);

# Run the cfitsio test.
check:
	(cd cfitsio; \
	make testprog; \
	./testprog > tmp.out 2>&1; \
	diff testprog.out tmp.out)

dist_starnews_DATA = cfitsio.news
