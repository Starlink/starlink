## Process this file with automake to produce Makefile.in

SUBDIRS = subs tasks sun137

dist_bin_SCRIPTS = pongo.icl pongowww 
nodist_bin_SCRIPTS = icl/pongo_proc.icl

bin_MONOLITHS = pongo_mon
dist_noinst_SCRIPTS = pongo_link_adam

starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)
nodist_pkgdata_DATA = $(PONGO_EXAMPLES)
dist_pkgdata_DATA = $(EXAMPLE_DATA_FILES)
dist_starnews_DATA = pongo.news
bindatadir = $(bindir)
bindata_DATA = $(DATA_FILES)

# Treat this as an A-task from the build system viewpoint since
# it only runs from ICL. Do not define _TASKS
pongo_mon_SOURCES = pongo_mon.f
pongo_mon_LDADD = tasks/libpongo_tasks.la subs/libpongo.la \
	`$(srcdir)/pongo_link_adam`

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

HELP_SOURCES = pongohelp.hlp

PONGO_EXAMPLES = icl/pongo_demo.icl icl/projections.icl	    \
icl/spectrum.icl icl/ppdotdiag.icl icl/histogram.icl		    \
icl/errors.icl icl/ellipses.icl icl/agi.icl icl/interactive.icl \
icl/cover.icl icl/figures.icl icl/radec.icl icl/vector.icl

PONGO_EXAMPLES_INFILES = $(PONGO_EXAMPLES:.icl=.icl.in)

EXAMPLE_DATA_FILES = data/dor.sdf data/cljon.dat data/ellipses.dat \
data/ppdot.dat data/tutorial.dat data/swp3196.lap data/highppm.dat \
data/gridlabels.dat

## All examples files need to be edited on installation.

edit = sed \
        -e 's,@PONGO_EXAMPLES\@,$(pkgdatadir),g'

%.icl: %.icl.in
	rm -f $@.tmp $@
	$(edit) \
	    $(srcdir)/$< > $(srcdir)/$@.tmp
	mv $(srcdir)/$@.tmp $(srcdir)/$@

## The above rule can get confused about pongo.icl and looks for pongo.icl.in.
pongo.icl: ;

## File not mentioned elsewhere that we need to distribute.
EXTRA_DIST = PONGO_PAR PONGO_CMN $(HELP_SOURCES) $(PONGO_EXAMPLES_INFILES) \
	icl/pongo_proc.icl.in

CLEANFILES = pongohelp.shl pongo_mon version.dat pongo_mon.ifc

