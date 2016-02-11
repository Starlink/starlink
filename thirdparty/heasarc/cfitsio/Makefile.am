## Process this file with automake to produce Makefile.in

## Library linking script.
dist_bin_SCRIPTS = cfitsio_link

## Create the imcopy, and fitscopy binaries, very useful.
bin_SCRIPTS = cfitsio/imcopy cfitsio/fitscopy

## The libraries, static and shared. Note shared needs special handling
## as automake doesn't like the unknown extension.
lib_LIBRARIES = cfitsio/libcfitsio.a
liblibdir = $(libdir)
liblib_SCRIPTS = cfitsio/libcfitsio@SHLIB_SUFFIX@ \
	cfitsio/libcfitsio.4@SHLIB_SUFFIX@ \
	cfitsio/libcfitsio.4.@PACKAGE_VERSION@@SHLIB_SUFFIX@ \
	cfitsio/cfitsio.pc

# There are no sources for libcfits in this directory.
cfitsio_libcfitsio_a_SOURCES =

include_HEADERS = \
    cfitsio/fitsio.h \
    cfitsio/longnam.h \
    cfitsio/fitsio2.h \
    cfitsio/drvrsmem.h

EXAMPLE_SOURCES = cfitsio/cookbook.c cfitsio/cookbook.f

# Put examples in dist_pkgdata
dist_pkgdata_DATA = $(EXAMPLE_SOURCES) cfitsio_changes.txt CFITSIO_CONDITIONS\
 License.txt

# Extra documentation for stardocs (not converted to hypertext).
stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ mud162.tex mud167.tex mud166.tex

# Headers and example sources are made within cfitsio
$(include_HEADERS) $(EXAMPLE_SOURCES):
	(cd cfitsio; export CFLAGS; unset CFLAGS; $(MAKE) all)

# Must be a separate target from above, otherwise automake doesn't notice,
# and adds its own, conflicting, target.
cfitsio/libcfitsio.a:
	(cd cfitsio; export CFLAGS; unset CFLAGS; $(MAKE) all)

$(liblib_SCRIPTS):
	(cd cfitsio; export CFLAGS; unset CFLAGS; $(MAKE) shared)

# The utility programs are created by cfitsio.
cfitsio/imcopy: cfitsio/imcopy.c cfitsio/libcfitsio.a
	(cd cfitsio; make imcopy )

cfitsio/fitscopy: cfitsio/fitscopy.c cfitsio/libcfitsio.a
	(cd cfitsio; make fitscopy )

#  Clean recurses into cfitsio.
clean-local:
	(cd cfitsio; make clean )

distclean-local:
	(cd cfitsio; make distclean )

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
