## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@ libcoi libcof libcon iraf

## We need the internal Figaro libraries. These are not currently generally
## available so this separation is a reminder this dependency will generally
## fail. 
FIGLIBS = -ldta -lfigich

bin_SCRIPTS = convert.csh convert.icl gif2ndf ndf2gif tiff2ndf convert.sh

dist_bin_SCRIPTS = convertndf convertawk mtfits2ndf.tcsh

dist_noinst_SCRIPTS = convert_link_adam iraf_link

if HAVE_WORKING_IRAF_IMFORT
  IRAFLIBS = `$(srcdir)/iraf_link`
else
  IRAFLIBS =
endif

bin_MONOLITHS = convert_mon

convert_mon_SOURCES = convert_mon.f $(TASKS:=.f) $(FPP_TASKS:=.F)
convert_mon_LDADD = libcoi/libcoi.la libcof/libcof.la libcon/libcon.la \
             iraf/libconspp.la $(FIGLIBS) $(IRAFLIBS) \
             `$(srcdir)/convert_link_adam`
convert_mon_TASKS = $(TASKS) $(FPP_TASKS)

FPP_TASKS = ndf2iraf iraf2ndf

TASKS = ascii2ndf ast2ndf conhelp da2ndf dst2ndf fits2ndf \
        gasp2ndf ircam2ndf ndf2ascii ndf2da ndf2dst ndf2fits ndf2gasp \
        ndf2pgm ndf2tiff ndf2unf specx2ndf unf2ndf

IFL_FILES = convert_mon.ifl $(TASKS:=.ifl)

HELP_FILES = package.hlp Parameters.hlp Running_from_a_Unix_Shell.hlp \
             Using_Help.hlp

#  List of ancillary data files (all types). These need a home at some point.
EXAMPLE_FILES = test_data/*

##  On-line help
HELP_SOURCES = convert.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

# Create help for the applications.
tasks.hlp: $(TASKS:=.f)
	-rm -f tasks.hlp
	cat $(TASKS:=.f) > all.f
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=all.f out=tasks.hlp single=no
	-rm all.f

convert.hlp: convert_master.hlp tasks.hlp $(HELP_FILES) 
	cat convert_master.hlp tasks.hlp $(HELP_FILES) > convert.hlp

##  Any misc files that have no where else.
dist_starnews_DATA = convert.news
dist_pkgdata_DATA = CONVERT_CONDITIONS

##  Edit scripts during make to have the correct paths.

edit = sed \
        -e 's,@bindir\@,$(bindir),g' \
        -e 's,@STARLINK\@,$(STARLINK),g' \
        -e 's,@PPMTOGIF\@,$(PPMTOGIF),g' \
        -e 's,@GIFTOPNM\@,$(GIFTOPNM),g' \
        -e 's,@PPMTOPGM\@,$(PPMTOPGM),g' \
        -e 's,@PNMFLIP\@,$(PPNFLIP),g' \
        -e 's,@PNMTOFITS\@,$(PNMTOFITS),g' \
        -e 's,@TIFFTOPNM\@,$(TIFFTOPNM),g' \
        -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g'

convert.csh: convert.csh.in Makefile
	rm -f convert.csh.tmp convert.csh
	$(edit) \
	   $(srcdir)/convert.csh.in >convert.csh.tmp
	mv convert.csh.tmp convert.csh

convert.sh: convert.sh.in Makefile
	rm -f convert.sh.tmp convert.sh
	$(edit) \
	   $(srcdir)/convert.sh.in >convert.sh.tmp
	mv convert.sh.tmp convert.sh

convert.icl: convert.icl.in Makefile
	rm -f convert.icl.tmp convert.icl
	$(edit) \
	   $(srcdir)/convert.icl.in >convert.icl.tmp
	mv convert.icl.tmp convert.icl

gif2ndf: gif2ndf.in Makefile
	rm -f gif2ndf.tmp gif2ndf
	$(edit) \
	   $(srcdir)/gif2ndf.in >gif2ndf.tmp
	mv gif2ndf.tmp gif2ndf

ndf2gif: ndf2gif.in Makefile
	rm -f ndf2gif.tmp ndf2gif
	$(edit) \
	   $(srcdir)/ndf2gif.in >ndf2gif.tmp
	mv ndf2gif.tmp ndf2gif

tiff2ndf: tiff2ndf.in Makefile
	rm -f tiff2ndf.tmp tiff2ndf
	$(edit) \
	   $(srcdir)/tiff2ndf.in >tiff2ndf.tmp
	mv tiff2ndf.tmp tiff2ndf

EXTRA_DIST = convert.f convert.csh.in convert.sh.in convert.icl.in gif2ndf.in \
             ndf2gif.in tiff2ndf.in $(HELP_SOURCES)

MISC_BUILT_FILES = convert.shl convert.hlp version.dat gif2ndf ndf2gif tiff2ndf \
                   convert.csh convert.icl convert_mon convert_mon.ifl convert.sh

IFC_FILES = conhelp.ifc da2ndf.ifc dst2ndf.ifc fits2ndf.ifc gasp2ndf.ifc iraf2ndf.ifc \
            ircam2ndf.ifc ndf2ascii.ifc ndf2da.ifc ndf2dst.ifc ndf2fits.ifc ndf2gasp.ifc \
            ndf2iraf.ifc ndf2pgm.ifc ndf2tiff.ifc ndf2unf.ifc specx2ndf.ifc unf2ndf.ifc \
            ascii2ndf.ifc ast2ndf.ifc convert_mon.ifc 

CLEANFILES = $(MISC_BUILT_FILES) $(IFC_FILES)

