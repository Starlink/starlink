## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = atools.csh atools.sh atools.icl astshow.tcl \
                   astviewer/astviewer/astviewer

dist_noinst_SCRIPTS = atools_link_adam

bin_MONOLITHS = atools_mon

atools_mon_SOURCES = \
        atools_mon.f \
	$(atools_mon_TASKS:=.f) \
	$(FSRC) \
	$(PRIVATE_INCLUDES)

atools_mon_LDADD = $(LDADD) `$(srcdir)/atools_link_adam`

atools_mon_TASKS = astcopy astaddframe astclear astget astset asttest \
 astcmpframe astcmpmap astframe astgetframe astgetmapping astunitmap \
 atlhelp astskyframe astframeset astpermmap astremoveframe astremapframe \
 astmatrixmap astconvert astlutmap astpcdmap astwinmap astshiftmap \
 astfindframe astmatchaxes astpermaxes astpickaxes astsimplify asttrann astmapbox \
 astinvert astspecframe asttimeframe astsetrefpos astgetrefpos astformat \
 astgetregbounds astgetregframe astunformat asttran1 asttran2 astmapregion astcmpregion \
 astmathmap astsetactunit astgetactunit astpolymap astswitchmap asttranmap \
 astdsbframe astfluxframe astsfluxframe astpolygon astellipse astmapsplit \
 astselectormap astinterval astbox astshowmesh astzoommap astdistance \
 astcircle astlinearapprox astnegate astremoveregion astdownsize \
 astoutline astrate astoverlap astmask asttrangrid astquadapprox \
 astratemap astwcsmap astpolytran astangle asttohds astfromhds astsphmap \
 astaddvariant astmirrorvars astprism astgetunc astconvex astchebymap \
 astchebydomain astmoc astaddpixelmask astaddregion astgetregdisc \
 astdecompose

FSRC = \
 atl1_gtfrm.f \
 atl1_ptobj.f atl1_setop.f atl1_gtofl.f \
 atl1_cpcol.f atl1_prnt2.f atl1_prnt1.f atl1_prntn.f


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
atools.csh atools.sh atools.icl $(atools_mon_TASKS:=.ifl): atools.ifd
	@IFD2STAR@ atools

##  Any misc ATOOLS files that have no where else.
dist_pkgdata_DATA = ATOOLS_CONDITIONS \
                    astviewer/astviewer/astviewer.html \
                    astviewer/astviewer/example-frameset.png \
                    astviewer/astviewer/frame.png  \
                    astviewer/astviewer/makecube.png  \
                    astviewer/astviewer/mapping.png  \
                    astviewer/astviewer/parallel.png

##  Things for distribution that are not picked up otherwise.
EXTRA_DIST = $(HELP_SOURCES) atools.star-hlp asttest.sh \
             skyframe.ast

##  Need to add generated files as BUILT_SOURCES.
BUILT_SOURCES = atools.csh atools.sh atools.icl

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

dist_starnews_DATA = atools.news


MISC_FILES = asttest dtask_applic.lo atools.shl history.help \
atools_mon version.dat atools.csh atools.icl atools.sh

IFC_FILES = $(addsuffix .ifc,$(atools_mon_TASKS)) atools_mon.ifc

IFL_FILES = $(addsuffix .ifl,$(atools_mon_TASKS)) atools_mon.ifl

CLEANFILES = $(MISC_FILES) $(IFC_FILES) $(IFL_FILES)
