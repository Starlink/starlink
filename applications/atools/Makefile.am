## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = atools.csh atools.icl astshow.tcl

dist_noinst_SCRIPTS = atools_link_adam

bin_MONOLITHS = atools_mon

atools_mon_SOURCES = \
        atools_mon.f \
	$(atools_mon_TASKS:=.f) \
	$(FSRC) \
	$(CSRC) \
	$(PRIVATE_INCLUDES)

atools_mon_LDADD = $(LDADD) `$(srcdir)/atools_link_adam`

atools_mon_TASKS = astcopy astaddframe astclear astget astset asttest \
 astcmpframe astcmpmap astframe astgetframe astgetmapping astunitmap \
 atlhelp astskyframe astframeset astpermmap astremoveframe astremapframe \
 astmatrixmap astconvert astlutmap astpcdmap astwinmap astshiftmap \
 astfindframe astpermaxes astpickaxes astsimplify asttrann astmapbox \
 astinvert astspecframe astsetrefpos astgetrefpos astformat asttran1 asttran2 \
 astmathmap astsetactunit astgetactunit astpolymap asttranmap astdsbframe \
 astfluxframe astsfluxframe

FSRC = \
 atl1_gtgrp.f atl1_rdgrp.f atl1_creat.f atl1_gtfrm.f atl1_gtobj.f \
 atl1_ptobj.f atl1_setop.f atl1_rdch.f atl1_rdfch.f atl1_gtofl.f \
 atl1_cpcol.f atl1_notif.f atl1_prnt2.f atl1_prnt1.f kpg1_gtgrp.f

CSRC = atl1_rm.c

##  The on-line help.
HELP_SOURCES = atools.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

prohlp=	@PROHLP@

atools.hlp: $(atools_mon_TASKS:=.f) history.help atools.star-hlp
	PATH="$(PATH):$(STARLINK)/bin/sst"; \
	export PATH ;\
	for f in $(atools_mon_TASKS:=.f); do \
	   $(prohlp) in=$${f} out=$${f}.help ;\
	done
	cat atools.star-hlp *.help > atools.hlp
	rm $(atools_mon_TASKS:=.f.help)

history.help: atools.news
	./make_history_help

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

# Rule for creating various components from the ifd file.
atools.csh atools.icl $(atools_mon_TASKS:=.ifl): atools.ifd
	@IFD2STAR@ atools

##  Any misc PISA files that have no where else.
dist_pkgdata_DATA = atools.news

##  Things for distribution that are not picked up otherwise.
EXTRA_DIST = $(HELP_SOURCES)

##  Need to add generated files as BUILT_SOURCES.
BUILT_SOURCES = atools.csh atools.icl

##  Simple check.
TESTS = asttest.sh

check_SCRIPTS = asttest.sh

check_MONOLITHS = asttest
asttest_SOURCES = asttest.f \
	$(FSRC) \
	$(CSRC) \
	$(PRIVATE_INCLUDES)

# no variable asttest_TASKS -- asttest is an ATASK
asttest_LDADD = $(LDADD) `$(srcdir)/atools_link_adam`
