## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libdvi2bitmap.la
D2BHEADERS = \
	Bitmap.h \
	BitmapImage.h \
	DviError.h \
	DviFile.h \
	GIFBitmap.h \
	InputByteStream.h \
	PageRange.h \
	PNGBitmap.h \
	PkFont.h \
	Util.h \
	XBMBitmap.h \
	XPMBitmap.h \
	kpathsea.h \
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
	PageRange.cc \
	PkFont.cc \
	Util.cc \
	XBMBitmap.cc \
	XPMBitmap.cc \
	$(D2BHEADERS)
EXTRA_libdvi2bitmap_la_SOURCES = \
	GIFBitmap.cc \
	PNGBitmap.cc \
	kpathsea.cc
EXTRALO=@EXTRALO@
libdvi2bitmap_la_LIBADD = $(EXTRALO) $(LTLIBOBJS)
libdvi2bitmap_la_DEPENDENCIES = $(EXTRALO) $(LTLIBOBJS)
# libdvi2bitmap_la_LIBADD = $(LTLIBOBJS)
# libdvi2bitmap_a_LIBADD = $(LIBOBJS)

bin_PROGRAMS = dvi2bitmap
dvi2bitmap_SOURCES = dvi2bitmap.cc
dvi2bitmap_LDADD = libdvi2bitmap.la

man1_MANS = dvi2bitmap.1

# Include both the manual and the library documentation in the distribution
EXTRA_DIST = $(libdocdir) test manual.tar $(man1_MANS) VERSION

# Generate library documentation using Doxygen
libdocdir = @DOXYGEN_OUTPUT_DIRECTORY@
$(libdocdir):
	doxygen

# doc/ directory has its own simple makefile
manual.tar: VERSION
	cd doc; $(MAKE) doc.tar
	mv doc/doc.tar ./manual.tar

VERSION:
	echo $(VERSION) >VERSION
