## Process this file with automake to produce Makefile.in

bin_SCRIPTS = datacube.csh datacube.icl datacube.sh

dist_bin_SCRIPTS = $(USERSCRIPTS) $(INTERNALSCRIPTS) 

# These are the public scripts of DATACUBE.  As compare_x becomes
# xcompare, place it in its command order so the help file will be in
# alphabetical order.
USERSCRIPTS = compare.csh gridspec.csh mapbyvel.csh multistack.csh \
              passband.csh peakmap.csh pvslice.csh ripper.csh \
              squash.csh stacker.csh step.csh trendview.csh \
              velmap.csh velmoment.csh compare_x.csh

# The are subroutine-like scripts used by the public scripts, 
# but are not part of the documented set of DATACUBE scripts.
INTERNALSCRIPTS = getcurpos.csh checkndf.csh getfitcon.csh \
                   make_fitgauss_table.csh

# stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

dist_starnews_DATA = datacube.news

# Create help for the scripts.  Convert concatenated scripts to 
# Fortran-like prologues then run PROHLP.  Make the names like
# their commands.
datacube.hlp: $(USERSCRIPTS) datacube_master.hlp
	-rm -f datacube.hlp temp.csh temp.csh temp.hlp temp2.hlp temp3.hlp
	cat $(USERSCRIPTS) > temp.csh
	sed -e '/^#/s/#/*/' temp.csh  > temp.f
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=temp.f out=temp.hlp single=no
	awk '{if ($$0 ~ /^1 /) print tolower(substr($$0,1,index($$0,".CSH")-1)); else print $$0}' temp.hlp > temp2.hlp
	awk '{if ($$0 ~ /^1 compare_x/) print "1 xcompare"; else print $$0}' temp2.hlp > temp3.hlp
	cat datacube_master.hlp temp3.hlp > datacube.hlp
	-rm temp.f temp.csh temp.hlp temp2.hlp temp3.hlp
	
#  The on-line help.
HELP_SOURCES = datacube.hlp

starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

SUBDIRS = demo help sc16 sun237

EXTRA_DIST = datacube_master.hlp $(HELP_SOURCES)

CLEANFILES = datacube.shl version.dat
