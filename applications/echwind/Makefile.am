## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_SCRIPTS = echwind.csh

bin_PROGRAMS = echwind
echwind_SOURCES = echwind.f
echwind_LDADD = libmodel/libmodel.la libplot/libplot.la libetc/libetc.la \
	`$(srcdir)/echwind_link`
echwind_FCFLAGS = -Ilibmodel

# Need to install the associated data files into per-package bindir
mydatadir = $(bindir)
mydata_DATA = balmer.dat detectors.dat echwind.ifl lines.dat spectrographs.dat


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
