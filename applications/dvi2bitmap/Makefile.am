## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libdvi2bitmap.la
D2BHEADERS = \
	Bitmap.h \
	BitmapImage.h \
	DviError.h \
	DviFile.h \
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
libdvi2bitmap_la_LIBADD = $(EXTRALO) $(LTLIBOBJS)
libdvi2bitmap_la_DEPENDENCIES = $(EXTRALO) $(LTLIBOBJS)

bin_PROGRAMS = dvi2bitmap dvireport
dvi2bitmap_SOURCES = dvi2bitmap.cc
dvi2bitmap_LDADD = libdvi2bitmap.la
dvireport_SOURCES = dvireport.cc
dvireport_LDADD = libdvi2bitmap.la

man1_MANS = dvi2bitmap.1 dvireport.1



# Special handling for distribution
#
# Include both the manual and the library documentation in the distribution
EXTRA_DIST = $(libdocdir) test manual.tar $(man1_MANS) VERSION

# The test directory has a couple of things to make prior to distribution
dist-hook:
	cd $(distdir)/test; $(MAKE) prepare-dist

# Generate library documentation using Doxygen
libdocdir = @DOXYGEN_OUTPUT_DIRECTORY@
$(libdocdir): Doxyfile
	doxygen

# doc/ directory has its own simple makefile
manual.tar: VERSION
	cd doc; $(MAKE) doc.tar
	mv doc/doc.tar ./manual.tar

VERSION: configure.ac
	echo $(VERSION) >VERSION
