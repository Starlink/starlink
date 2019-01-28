## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = atools.csh atools.sh atools.icl astshow.tcl astviewer

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
 astchebydomain astmoc astaddpixelmask astaddregion

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
dist_pkgdata_DATA = ATOOLS_CONDITIONS astviewer.html example-frameset.png \
                    frame.png  makecube.png  mapping.png  parallel.png

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

IFC_FILES = astaddframe.ifc astclear.ifc astcmpframe.ifc astcmpmap.ifc \
astratemap.ifc astconvert.ifc astcopy.ifc astdsbframe.ifc astellipse.ifc \
astfindframe.ifc astmatchaxes.ifc astfluxframe.ifc astformat.ifc astunformat.ifc astframe.ifc \
astframeset.ifc astgetactunit.ifc astgetframe.ifc astget.ifc \
astgetmapping.ifc astgetrefpos.ifc astinvert.ifc astlutmap.ifc \
astmapbox.ifc astmathmap.ifc astmatrixmap.ifc astpcdmap.ifc astbox.ifc \
astpermaxes.ifc astpermmap.ifc astpickaxes.ifc astpolygon.ifc \
astinterval.ifc astpolymap.ifc astremapframe.ifc astremoveframe.ifc \
astsetactunit.ifc astset.ifc astsetrefpos.ifc astsfluxframe.ifc \
astshiftmap.ifc astsimplify.ifc astskyframe.ifc asttimeframe.ifc astspecframe.ifc \
asttest.ifc asttran1.ifc asttran2.ifc astswitchmap.ifc asttranmap.ifc asttrann.ifc \
astunitmap.ifc astwinmap.ifc atlhelp.ifc astmapsplit.ifc atools_mon.ifc \
astselectormap.ifc astmapregion.ifc astcmpregion.ifc astgetregbounds.ifc \
astgetregframe.ifc astshowmesh.ifc astzoommap.ifc astdistance.ifc astcircle.ifc \
astlinearapprox.ifc astnegate.ifc astremoveregion.ifc astdownsize.ifc \
astoutline.ifc astrate.ifc astoverlap.ifc astmask.ifc asttrangrid.ifc \
astquadapprox.ifc astwcsmap.ifc astangle.ifc asttohds.ifc astfromhds.ifc \
astsphmap.ifc astaddvariant.ifc astmirrorvars.ifc astprism.ifc astgetunc.ifc \
astconvex.ifc astchebymap.ifc astchebydomain.ifc astmoc.ifc \
astaddpixelmask.ifc astaddregion.ifc

IFL_FILES = astaddframe.ifl astclear.ifl astcmpframe.ifl astcmpmap.ifl \
astratemap.ifl astconvert.ifl astcopy.ifl astdsbframe.ifl astellipse.ifl \
astfindframe.ifl astmatchaxes.ifl astfluxframe.ifl astformat.ifl astunformat.ifl astframe.ifl \
astframeset.ifl astgetactunit.ifl astgetframe.ifl astget.ifl \
astgetmapping.ifl astgetrefpos.ifl astinvert.ifl astlutmap.ifl \
astmapbox.ifl astmathmap.ifl astmatrixmap.ifl astpcdmap.ifl astbox.ifl \
astpermaxes.ifl astpermmap.ifl astpickaxes.ifl astpolygon.ifl \
astinterval.ifl astpolymap.ifl astremapframe.ifl astremoveframe.ifl \
astsetactunit.ifl astset.ifl astsetrefpos.ifl astsfluxframe.ifl \
astshiftmap.ifl astsimplify.ifl astskyframe.ifl astspecframe.ifl asttimeframe.ifl \
asttest.ifl asttran1.ifl asttran2.ifl astswitchmap.ifl asttranmap.ifl asttrann.ifl \
astmapsplit.ifl astunitmap.ifl astwinmap.ifl atlhelp.ifl atools_mon.ifl \
astselectormap.ifl astmapregion.ifl astcmpregion.ifl astgetregbounds.ifl \
astgetregframe.ifl astshowmesh.ifl astzoommap.ifl astdistance.ifl astcircle.ifl \
astlinearapprox.ifl astnegate.ifl astremoveregion.ifl astdownsize.ifl \
astoutline.ifl astrate.ifl astoverlap.ifl astmask.ifl asttrangrid.ifl \
astquadapprox.ifl astwcsmap.ifl astangle.ifl asttohds.ifl astfromhds.ifl \
astsphmap.ifl astaddvariant.ifl astmirrorvars.ifl astprism.ifl astgetunc.ifl \
astconvex.ifl astchebymap.ifl astchebydomain.ifl astmoc.ifl \
astaddpixelmask.ifl astaddregion.ifl

CLEANFILES = $(MISC_FILES) $(IFC_FILES) $(IFL_FILES)
