## Process this file with automake to produce Makefile.in

## Library linking script.
bin_SCRIPTS = cfitsio_link
dist_bin_SCRIPTS = cfitsio_link

## CFITSIO builds this library itself, but we need to make a copy available. 
## XXX what about libtool and shareable versions? Different file types.
lib_LIBRARIES = libcfitsio.a

## There are no sources for this.
libcfitsio_a_SOURCES =

include_HEADERS = fitsio.h longnam.h

EXAMPLE_SOURCES = cookbook.c cookbook.f

## Put examples in dist_pkgdata
dist_pkgdata_DATA = $(EXAMPLE_SOURCES) cfitsio_changes.txt CFITSIO_CONDITIONS\
 Licence.txt cfitsio.news

## These are the things we require from cfitsio.
BUILT_SOURCES = libcfitsio.a fitsio.h longnam.h $(EXAMPLE_SOURCES)

libcfitsio.a: cfitsio/libcfitsio.a
	cp -p cfitsio/libcfitsio.a .

cfitsio/libcfitsio.a:
	(cd cfitsio; unset CFLAGS ; make)

fitsio.h: cfitsio/fitsio.h
	cp -p cfitsio/fitsio.h .

longnam.h: cfitsio/longnam.h
	cp -p cfitsio/longnam.h .

$(EXAMPLE_SOURCES):
	cp -p cfitsio/$@ .

##  Clean recurses into cfitsio.
clean-local:
	(cd cfitsio; make clean )

distclean-local:
	(cd cfitsio; make distclean )

## Clean up extra generated files.
MAINTAINERCLEANFILES = $(include_HEADERS) $(EXAMPLE_SOURCES)

##  Need all the source code in the cfitsio sub-directory.
EXTRA_DIST = cfitsio

##  Before distributing the subdirectory we need to clean it up (we get all
##  the files). Also remove the CVS trace.
dist-hook:
	rm -rf `find $(distdir) -name CVS`
	(cd $(distdir)/cfitsio; make distclean);
