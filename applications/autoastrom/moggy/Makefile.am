## Process this file with automake to produce Makefile.in

bin_PROGRAMS = moggy
moggy_SOURCES = \
	moggy.C \
	AstHandler.C \
	CommandParse.C \
	CatalogueHandler.C \
	util.C \
	moggy.h \
	AstHandler.h \
	CommandParse.h \
	CatalogueHandler.h \
	util.h \
	stringstream.h \
	verbosity.h
moggy_LDADD = $(LIBOBJS) -lm -lcnf
#moggy_LDADD = $(LIBOBJS) -lm `cnf_link`

EXTRA_DIST = moggy-doc.txt LICENCE t
