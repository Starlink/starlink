## Process this file with automake to produce Makefile.in

SUBDIRS = libmodel libplot libetc

bin_SCRIPTS = echwind.csh

dist_noinst_SCRIPTS = echwind_link

bin_PROGRAMS = echwind
echwind_SOURCES = echwind.F
echwind_LDADD = libmodel/libmodel.la libplot/libplot.la libetc/libetc.la \
	`$(srcdir)/echwind_link`
echwind_FCFLAGS = -Ilibmodel

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Need to install the associated data files into per-package bindir
mydatadir = $(bindir)
mydata_DATA = balmer.dat detectors.dat echwind.ifl lines.dat spectrographs.dat

PRIVATE_INCLUDES = echwind_par echwind_orders echwind_mmpix echwind_dets \
                   echwind_lines 

# Fixup scripts with bin location
# The scripts needs to have installation locations edited into them
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@pgplotdir\@,$(exec_prefix)/bin,g' \
	-e 's,@PACKAGE_VERSION\@,$(VERSION),g'

echwind.csh: echwind.csh.in
	-rm -f echwind.csh.tmp echwind.csh
	$(edit) \
	    $(srcdir)/echwind.csh.in > echwind.csh.tmp
	mv echwind.csh.tmp echwind.csh

EXTRA_DIST =  $(PRIVATE_INCLUDES) echwind.csh.in balmer.dat \
              detectors.dat echwind.ifl lines.dat spectrographs.dat
              
dist_starnews_DATA = echwind.news
