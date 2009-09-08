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
../jty/libfigjty.la \
../tio/libfigtio.la \
../par/libfigpar.la \
../gen/libfiggen.la \
../ich/libfigich.la \
`pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` `sla_link` `fio_link_adam`

IFC_FILES = arcsdi.ifc cadd.ifc changed.ifc comb.ifc crigauss.ifc \
cscan.ifc csub.ifc cube2long.ifc fib2cube.ifc fibdisp.ifc \
fibsep.ifc fitcont.ifc himage.ifc iscan.ifc longslit.ifc \
vig.ifc arc2d.ifc figaro5.ifc

MISC_BUILT_FILES = version.dat figaro5 figaro5.ifl

CLEANFILES = $(IFC_FILES) $(MISC_BUILT_FILES)
