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
../jty/libjty.la \
../gen/libfiggen.la \
../gkd/libgkd.la \
../par/libfigpar.la \
../ich/libfigich.la \
`pda_link`  `agp_link_adam` `grp_link_adam` `ndf_link_adam` \
`fio_link_adam` 
