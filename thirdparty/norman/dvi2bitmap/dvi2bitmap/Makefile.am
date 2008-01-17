## Process this file with automake to produce Makefile.in

## Build this directory before its children (required, since test programs
## link against ../libdvi2bitmap.la).  This results in the test programs
## always being built, rather than at 'make check' time, but it appears
## to be necessary in order for 'make check' and 'make dist' to include
## the test directory.
SUBDIRS = . doc test

lib_LTLIBRARIES = libdvi2bitmap.la
D2BHEADERS = \
	Bitmap.h \
	BitmapImage.h \
	DviError.h \
	DviFile.h \
	DviFilePosition.h \
	InputByteStream.h \
	FileByteStream.h \
	PipeStream.h \
	PageRange.h \
	PkFont.h \
	Util.h \
	XBMBitmap.h \
	XPMBitmap.h \
	Byte.h \
	getopt_long.h \
	stringstream.h \
	verbosity.h
libdvi2bitmap_la_SOURCES = \
	Bitmap.cc \
	BitmapImage.cc \
	DviError.cc \
	DviFile.cc \
	DviFilePosition.cc \
	InputByteStream.cc \
	FileByteStream.cc \
	PipeStream.cc \
	PageRange.cc \
	PkFont.cc \
	Util.cc \
	XBMBitmap.cc \
	XPMBitmap.cc \
	$(D2BHEADERS)
EXTRA_libdvi2bitmap_la_SOURCES = \
	GIFBitmap.cc \
	GIFBitmap.h \
	PNGBitmap.cc \
	PNGBitmap.h \
	KarlPathSearcher.cc \
	KarlPathSearcher.h
EXTRALO=@EXTRALO@
libdvi2bitmap_la_LIBADD = $(EXTRALO) $(LTLIBOBJS) -lm
libdvi2bitmap_la_DEPENDENCIES = $(EXTRALO) $(LTLIBOBJS)
libdvi2bitmap_la_LDFLAGS = -version-info @libdvi2bitmap_la_version_info@

bin_PROGRAMS = dvi2bitmap dvireport
dvi2bitmap_SOURCES = dvi2bitmap.cc
dvi2bitmap_LDADD = libdvi2bitmap.la
dvireport_SOURCES = dvireport.cc
dvireport_LDADD = libdvi2bitmap.la

man1_MANS = dvi2bitmap.1 dvireport.1

%.gz: %
	gzip --best --force $<

# Special handling for distribution
EXTRA_DIST = $(libdocdir) $(man1_MANS) VERSION \
    doc/dvi2bitmap-docs.tar.gz doc/dvi2bitmap-docs.pdf

# Generate library documentation using Doxygen.
# If it's not available, just touch a stamp file in the right directory.
# The configure.ac sets DOXYGEN to NO_DOXYGEN if it's not found.
libdocdir = @DOXYGEN_OUTPUT_DIRECTORY@
DOXYGEN=@DOXYGEN@
$(libdocdir): Doxyfile
	if test -d $(libdocdir); then \
	  rm -Rf $(libdocdir); \
	fi
	if test "$(DOXYGEN)" != NO_DOXYGEN; then \
	  $(DOXYGEN); \
	else \
	  mkdir $(libdocdir); \
	  echo "No doxygen available -- library documentation not generated" >$(libdocdir)/stamp; \
	fi

doc/dvi2bitmap-docs.tar.gz: VERSION
	cd doc; make dvi2bitmap-docs.tar.gz
doc/dvi2bitmap-docs.pdf: VERSION
	cd doc; make dvi2bitmap-docs.pdf
test/sampler.tar:
	cd test; make sampler.tar

# Tarball of materials for uploading to the webpage
webpage-tarball.tar: \
		html/index.html \
		doc/dvi2bitmap-docs.tar.gz \
		doc/dvi2bitmap-docs.pdf \
		test/sampler.tar \
		$(libdocdir)
	rm -Rf dvi2bitmap-webpage
	mkdir dvi2bitmap-webpage
	cp html/index.html html/style.css doc/dvi2bitmap-docs.pdf \
		dvi2bitmap-webpage
	cd dvi2bitmap-webpage; tar xzf ../doc/dvi2bitmap-docs.tar.gz
	cd dvi2bitmap-webpage; tar xf ../test/sampler.tar
	pax -r -w $(libdocdir) dvi2bitmap-webpage
	cd dvi2bitmap-webpage; tar cf ../$@ .
	rm -Rf dvi2bitmap-webpage

VERSION: configure.ac
	echo $(VERSION) >VERSION
