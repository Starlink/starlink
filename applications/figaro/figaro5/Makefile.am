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
../opt/libopt.la \
../twoutil/libtwoutil.la \
../graph/libgraph.la \
../par/libfigpar.la \
../fibre/libfibre.la \
../fda/libfda.la \
../dta/libdta.la \
../fig/libfig.la \
../gen/libgen.la \
../unix/libunix.la \
../ich/libich.la \
$(EXTERNAL_LIBS)


EXTERNAL_LIBS = `pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` \
         `fio_link_adam`
