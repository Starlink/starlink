## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro5

figaro5_SOURCES = figaro5.f \
                  $(figaro5_TASKS:=.f)

figaro5_TASKS = \
arc2d arcsdi cadd changed comb crigauss cscan csub cube2long fib2cube \
fibdisp fibsep fitcont himage iscan longslit vig

figaro5_LDADD = $(LDADD) \
../main/libmain.la \
../opt/libopt.la \
../twoutil/libtwoutil.la \
../graph/libgraph.la \
../unix/libunix.la \
../fibre/libfibre.la \
../fda/libfda.la \
../fig/libfig.la \
../jty/libjty.la \
../tio/libfigtio.la \
../dsa/libdsa.la \
../dta/libdta.la \
../par/libfigpar.la \
../gen/libfiggen.la \
../ich/libfigich.la \
`pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` `sla_link` `fio_link_adam`
