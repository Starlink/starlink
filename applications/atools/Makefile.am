## Process this file with automake to produce Makefile.in

SUBDIRS = sun273

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

atools_mon_TASKS = \
 astaddframe \
 astaddpixelmask \
 astaddregion \
 astaddvariant \
 astangle \
 astbox \
 astchebydomain \
 astchebymap \
 astcircle \
 astclear \
 astcmpframe \
 astcmpmap \
 astcmpregion \
 astconvert \
 astconvex \
 astcopy \
 astdecompose \
 astdistance \
 astdownsize \
 astdsbframe \
 astellipse \
 astfindframe \
 astfluxframe \
 astformat \
 astframe \
 astframeset \
 astfromhds \
 astget \
 astgetactunit \
 astgetframe \
 astgetmapping \
 astgetrefpos \
 astgetregbounds \
 astgetregdisc \
 astgetregframe \
 astgetunc \
 astinterval \
 astinvert \
 astlinearapprox \
 astlutmap \
 astmapbox \
 astmapregion \
 astmapsplit \
 astmask \
 astmatchaxes \
 astmathmap \
 astmatrixmap \
 astmirrorvars \
 astmoc \
 astnegate \
 astoutline \
 astoverlap \
 astpcdmap \
 astpermaxes \
 astpermmap \
 astpickaxes \
 astpolygon \
 astpolymap \
 astpolytran \
 astprism \
 astquadapprox \
 astrate \
 astratemap \
 astremapframe \
 astremoveframe \
 astremoveregion \
 astselectormap \
 astset \
 astsetactunit \
 astsetrefpos \
 astsfluxframe \
 astshiftmap \
 astshowmesh \
 astsimplify \
 astskyframe \
 astspecframe \
 astsphmap \
 astswitchmap \
 asttest \
 asttimeframe \
 asttohds \
 asttran1 \
 asttran2 \
 asttrangrid \
 asttranmap \
 asttrann \
 astunformat \
 astunitmap \
 astwcsmap \
 astwinmap \
 astzoommap \
 atlhelp

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

# Rule to prepare LaTeX documenation to include in SUN/273.
atools_mon.tex: $(atools_mon_TASKS:=.f)
	rm -f $@
	cat $(atools_mon_TASKS:=.f) > all.f
	PATH="$(PATH):$(STARLINK)/bin/sst"; \
	export PATH ;\
	@PROLAT@ in=all.f out=$@ single=no page=yes atask=yes document=no
	rm all.f

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
