## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro5

figaro5_SOURCES = figaro5.f \
                  $(figaro5_TASKS:=.f)

figaro5_TASKS = arc2d cadd comb cscan cube2long fibdisp himage	    \
longslit arcsdi changed crigauss csub fib2cube fibsep fitcont iscan \
vig


figaro5_LDADD = \
../main/libmain.la \
../par/libfigpar.la \
../fda/libfda.la \
../dta/libdta.la \
../ich/libich.la \
../gen/libgen.la \
../opt/libopt.la \
../twoutil/libtwoutil.la \
../graph/libgraph.la \
../fig/libfig.la \
../fibre/libfibre.la \
../unix/libunix.la \
$(EXTERNAL_LIBS)


EXTERNAL_LIBS = `pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` \
         `fio_link_adam`
