## Process this file with automake to produce Makefile.in

SUBDIRS = cupidsub

dist_bin_SCRIPTS = cupid.csh cupid.sh cupid.icl outlineclump.sh

bindatadir = $(bindir)
dist_bindata_DATA = $(DATA_FILES)

DATA_FILES = test_data.sdf test_data.FIT

dist_noinst_SCRIPTS = cupid_link_adam make_doc make_h

bin_MONOLITHS = cupid_mon

include_MESSAGES = cupid_err.h CUPID_ERR

## Ensure we use the 8-byte NDF interface
AM_CFLAGS = -DNDF_I8

cupid_mon_SOURCES = \
        cupid.h \
        cupid_mon.c \
	$(cupid_mon_TASKS:=.c) \
	$(CSRC) \
	$(PRIVATE_INCLUDES)

cupid_mon_LDADD = $(LDADD) cupidsub/libcupidsub.la `$(srcdir)/cupid_link_adam`
## cupid_mon_LDFLAGS = -static
cupid_mon_TASKS = clumpinfo extractclumps findback findclumps cupidhelp makeclumps

CSRC =

##  The on-line help.
HELP_SOURCES = cupid.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

prohlp=	@PROHLP@

cupid.hlp: $(cupid_mon_TASKS:=.c) history.help outlineclump.help cupid.star-hlp
	PATH="$(PATH):$(STARLINK)/bin/sst"; \
	export PATH ;\
	for f in $(cupid_mon_TASKS:=.c); do \
	   $(prohlp) in=$${f} out=$${f}.help ;\
	done
	cat cupid.star-hlp *.help > cupid.hlp
	rm $(cupid_mon_TASKS:=.c.help) outlineclump.help

history.help: cupid.news
	./make_history_help

cupid.h: cupid.h.source make_h
	./make_h

outlineclump.help: outlineclump.sh
	cat outlineclump.sh | sed -e 's@^#@*@' > outlineclump.f
	-rm -f outlineclump.help
	PATH="$(PATH):$(STARLINK)/bin/sst"; \
	export PATH ;\
	$(prohlp) in=outlineclump.f out=outlineclump.help
	rm -f outlineclump.f

##  The latex documentation

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
prolat= @PROLAT@

sun255.tex: $(cupid_mon_TASKS:=.c) sun_master.tex
	PATH="$(PATH):$(STARLINK)/bin/sst"; \
	export PATH ;\
	for f in $(cupid_mon_TASKS:=.c); do \
	   $(prolat) in=$${f} atask nodocument nopage single out=$${f}.latex ;\
	   ./make_quickdes $${f} ;\
	done
	cat outlineclump.sh | sed -e 's@^#@*@' > outlineclump.f
	PATH="$(PATH):$(STARLINK)/bin/sst"; \
	export PATH ;\
	$(prolat) in=outlineclump.f atask nodocument nopage single out=outlineclump.latex
	./make_quickdes outlineclump.f
	rm -f outlineclump.f
	cat *.latex > tasks.tex
	cat *.quickdes > list.tex
	rm $(cupid_mon_TASKS:=.c.latex) outlineclump.latex *.quickdes
	./make_doc
	rm tasks.tex list.tex

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

# Rule for creating various components from the ifd file.
cupid.csh cupid.sh cupid.icl $(cupid_mon_TASKS:=.ifl): cupid.ifd
	@IFD2STAR@ cupid

##  Things for distribution that are not picked up otherwise.
EXTRA_DIST = $(HELP_SOURCES) cupid.star-hlp cupidtest.sh cupid.h.source \
       sun_master.tex

##  Need to add generated files as BUILT_SOURCES.
BUILT_SOURCES = cupid.csh cupid.sh cupid.icl cupid.h $(include_MESSAGES)

##  Simple check.
TESTS = cupidtest.sh

check_SCRIPTS = cupidtest.sh

dist_starnews_DATA = cupid.news
dist_pkgdata_DATA = LICENCE

MISC_FILES = dtask_applic.lo cupid.shl history.help \
cupid_mon version.dat cupid.csh cupid.icl cupid.sh

IFC_FILES = clumpinfo.ifc extractclumps.ifc findback.ifc findclumps.ifc makeclumps.ifc cupidhelp.ifc cupid_mon.ifc

IFL_FILES = $(IFC_FILES:.ifc=.ifl)

MAINTAINERCLEANFILES = cupid.h
CLEANFILES = $(MISC_FILES) $(IFC_FILES) $(IFL_FILES) cupid.h


