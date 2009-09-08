## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro3

figaro3_SOURCES = figaro3.f \
                  $(NORMALTASKS:=.f)

figaro3_TASKS = $(NORMALTASKS) $(CODELESSTASKS)

NORMALTASKS = \
arc cdist echarc echfind echmask echmerge echselect findsp flaircomp \
flairext iarc iscrunch maskext offdist overpf polext sdist skyliner \
vachel xcopi xcopy

CODELESSTASKS = iscruni 

figaro3_LDADD = $(LDADD) \
../appsub/libappsub.la \
../fig/libfig.la \
../tio/libfigtio.la \
../fla/libfla.la \
../fda/libfda.la \
../jty/libfigjty.la \
../gen/libfiggen.la \
../gkd/libgkd.la \
../par/libfigpar.la \
../ich/libfigich.la \
`pda_link`  `agi_link_adam` `grp_link_adam` `ndf_link_adam` \
`fio_link_adam` 

IFC_FILES = echselect.ifc findsp.ifc flaircomp.ifc flairext.ifc iarc.ifc \
iscrunch.ifc iscruni.ifc maskext.ifc offdist.ifc overpf.ifc \
polext.ifc sdist.ifc skyliner.ifc vachel.ifc xcopi.ifc xcopy.ifc \
arc.ifc cdist.ifc echarc.ifc echfind.ifc echmask.ifc echmerge.ifc \
figaro3.ifc

MISC_BUILT_FILES = version.dat figaro3 figaro3.ifl

CLEANFILES = $(IFC_FILES) $(MISC_BUILT_FILES)



