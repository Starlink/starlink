## Process this file with automake to produce Makefile.in

bin_MONOLITHS = observe
observe_SOURCES = observe.f

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

dist_noinst_SCRIPTS = observe_link_adam

observe_LDADD = $(LDADD) `$(srcdir)/observe_link_adam`

TESTS = observe.dat

##  observe news file.
dist_starnews_DATA = observe.news

CLEANFILES = observe.ifc
