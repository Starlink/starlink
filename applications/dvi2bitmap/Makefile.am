## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libdvi2bitmap.la
libdvi2bitmap_la_SOURCES 	= \
				Bitmap.cc \
				BitmapImage.cc \
				DviError.cc \
				DviFile.cc \
				GIFBitmap.cc \
				InputByteStream.cc \
				PageRange.cc \
				PNGBitmap.cc \
				PkFont.cc \
				Util.cc \
				XBMBitmap.cc \
				XPMBitmap.cc \
				kpathsea.cc

bin_PROGRAMS = dvi2bitmap
dvi2bitmap_SOURCES = dvi2bitmap.cc
dvi2bitmap_LDADD = libdvi2bitmap.la

man1_MANS = dvi2bitmap.1

# Include both the manual and the library documentation in the distribution
EXTRA_DIST = $(libdocdir) manual.tar VERSION

# Generate library documentation using Doxygen
libdocdir = @DOXYGEN_OUTPUT_DIRECTORY@
$(libdocdir):
	doxygen

# doc/ directory has its own simple makefile
manual.tar:
	cd doc; $(MAKE) doc.tar
	mv doc/doc.tar ./manual.tar

VERSION:
	echo $(VERSION) >VERSION
