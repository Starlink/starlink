## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro3

figaro3_SOURCES = figaro3.f $(F_ROUTINES)

figaro3_TASKS = arc cdist echarc echfind echmask echmerge echselect    \
findsp flaircomp flairext iarc iscrunch iscruni maskext offdist overpf \
polext sdist skyliner vachel xcopi xcopy

# Not a one-to-one correspondence of fortran to tasks
F_ROUTINES = arc.f echarc.f echmask.f echselect.f findsp.f flairext.f \
iscrunch.f offdist.f polext.f skyliner.f xcopi.f cdist.f echfind.f    \
echmerge.f flaircomp.f iarc.f maskext.f overpf.f sdist.f    \
vachel.f xcopy.f

figaro3_LDADD = \
../par/libfigpar.la \
../fla/libfla.la \
../dta/libdta.la \
../appsub/libappsub.la \
../fig/libfig.la \
../jty/libjty.la \
../gen/libgen.la \
../gkd/libgkd.la \
../fda/libfda.la \
../ich/libich.la \
$(EXTERNAL_LIBS)

EXTERNAL_LIBS = `pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` \
         `fio_link_adam`
