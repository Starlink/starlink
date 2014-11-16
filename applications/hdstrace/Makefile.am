## Process this file with automake to produce Makefile.in

SUBDIRS = tra

bin_MONOLITHS = hdstrace
hdstrace_SOURCES = hdstrace.f

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

##  The on-line help.
HELP_SOURCES = hdstrace.hlp top_help.hlp

starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<


# no variable hdstrace_TASKS -- hdstrace is an ATASK
hdstrace_LDADD = $(LDADD) tra/libtra.la `prm_link_adam` `hds_link_adam` `fio_link_adam` `par_link_adam` `one_link`

TESTS = hdtest
dist_check_SCRIPTS = hdtest

dist_starnews_DATA = hdstrace.news
dist_pkgdata_DATA = LICENCE

EXTRA_DIST = hdstrace_test.sdf $(HELP_SOURCES)

CLEANFILES = hdstrace.ifc

