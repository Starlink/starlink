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
libdvi2bitmap_la_LIBADD = $(EXTRALO) $(LTLIBOBJS) -lm
libdvi2bitmap_la_DEPENDENCIES = $(EXTRALO) $(LTLIBOBJS)
libdvi2bitmap_la_LDFLAGS = -version-info @libdvi2bitmap_la_version_info@

bin_PROGRAMS = dvi2bitmap dvireport
dvi2bitmap_SOURCES = dvi2bitmap.cc
dvi2bitmap_LDADD = libdvi2bitmap.la
dvireport_SOURCES = dvireport.cc
dvireport_LDADD = libdvi2bitmap.la

man1_MANS = dvi2bitmap.1 dvireport.1



# Special handling for distribution
EXTRA_DIST = $(libdocdir) $(man1_MANS) VERSION

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

VERSION: configure.ac
	echo $(VERSION) >VERSION
