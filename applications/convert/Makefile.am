## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

## We need the internal Figaro libraries. These are not currently available so
## this is a terrible hack. Ideally all packages should probably put their
## useful libraries in libpkgdir
FIGLIBS = ../figaro/dta/libdta.la ../figaro/ich/libich.la

## The IRAF libraries are platform dependent, so we must be prepared to deal
## with their absence.
IRAFLIBS=@IRAFLIBS@

bin_SCRIPTS = convert.csh convert.icl gif2ndf ndf2gif tiff2ndf mtfits2ndf.tcsh\
 convertndf convertawk

LOCAL_LINK = coi/libcoi.la cof/libcof.la con/libcon.la aif/libaif.la \
             iraf/libconspp.la $(FIGLIBS) $(IRAFLIBS) \
             `$(srcdir)/convert_link_adam`

bin_MONOLITHS = convert_mon

convert_mon_SOURCES = convert_mon.fpp $(TASKS:=.f)
convert_mon_LDADD = $(LOCAL_LINK)
convert_mon_TASKS = $(TASKS)

TASKS = ascii2ndf ast2ndf conhelp da2ndf dst2ndf fits2ndf \
 gasp2ndf ircam2ndf ndf2ascii ndf2da ndf2dst ndf2fits ndf2gasp \
 ndf2pgm ndf2tiff ndf2unf specx2ndf unf2ndf iraf2ndf ndf2iraf

IFL_FILES = convert_mon.ifl $(TASKS:=.ifl)

#  List of ancillary data files (all types). These need a home at some point.
EXAMPLE_FILES = test_data/*

##  On-line help
HELP_FILES = convert.hlp

##  Edit scripts during make to have the correct paths.

edit = sed \
        -e 's,@bindir\@,$(bindir),g' \
        -e 's,@PPMTOGIF\@,$(PPMTOGIF),g' \
        -e 's,@GIFTOPNM\@,$(GIFTOPNM),g' \
        -e 's,@PPMTOPGM\@,$(PPMTOPGM),g' \
        -e 's,@PNMFLIP\@,$(PPNFLIP),g' \
        -e 's,@PNMTOFITS\@,$(PNMTOFITS),g' \
        -e 's,@TIFFTOPNM\@,$(TIFFTOPNM),g' \
        -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g'

convert.csh: convert.csh.in
	rm -f convert.csh.tmp convert.csh
	$(edit) \
	   $(srcdir)/convert.csh.in >convert.csh.tmp
	mv convert.csh.tmp convert.csh

convert.icl: convert.icl.in
	rm -f convert.icl.tmp convert.icl
	$(edit) \
	   $(srcdir)/convert.icl.in >convert.icl.tmp
	mv convert.icl.tmp convert.icl

gif2ndf: gif2ndf.in
	rm -f gif2ndf.tmp gif2ndf
	$(edit) \
	   $(srcdir)/gif2ndf.in >gif2ndf.tmp
	mv gif2ndf.tmp gif2ndf

ndf2gif: ndf2gif.in
	rm -f ndf2gif.tmp ndf2gif
	$(edit) \
	   $(srcdir)/ndf2gif.in >ndf2gif.tmp
	mv ndf2gif.tmp ndf2gif

tiff2ndf: tiff2ndf.in
	rm -f tiff2ndf.tmp tiff2ndf
	$(edit) \
	   $(srcdir)/tiff2ndf.in >tiff2ndf.tmp
	mv tiff2ndf.tmp tiff2ndf

