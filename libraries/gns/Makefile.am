## Process this file with automake to produce Makefile.in

bin_SCRIPTS = gns_link gns_link_adam

lib_LTLIBRARIES = libgns.la

libgns_la_SOURCES = $(F_ROUTINES) $(FPP_ROUTINES) $(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES)

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it.

PUBLIC_INCLUDES = GNS_ERR GNS_PAR

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it. Note that these are in
#  lower case.

PRIVATE_INCLUDES =  gns.cmn chars.par

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files.

F_ROUTINES = agin.f decsp.f filtg.f filti.f gtn.f gwmov.f gwng.f   \
gwni.f hostn.f iang.f iani.f idchi.f idng.f ietg.f igag.f iiai.f   \
intin.f iong.f itwcg.f iwcg.f iwsg.f logtr.f msg.f rdwsi.f rdwst.f \
spech.f start.f stop.f termg.f termi.f tndg.f tng.f tni.f trim.f   \
fndf.f

FPP_ROUTINES = initg.fpp initi.fpp

BUILT_SOURCES = GNS_ERR

# Need to specify how to create the binary database files

# Not sure if this is the correct way of building helper applications
# that are neither for testing nor for final install. Maybe there is
# a local version of bin_PROGRAMS

gksbuild: gksbuild.o
	$(FCLINK) gksbuild.o 

idibuild: idibuild.o
	$(FCLINK) idibuild.o

gns_gksdevices : gksbuild gksdevices.txt
	-rm gns_gksdevices
	echo gksdevices.txt | ./gksbuild
	chmod o+r gns_gksdevices

gns_ididevices : idibuild ididevices.txt
	-rm gns_ididevices
	echo ididevices.txt | ./idibuild
	chmod o+r gns_ididevices

# Tests

TESTS = gns_test gnsrun_gks

check_PROGRAMS = gns_test gnsrun_gks

gnsrun_gks_SOURCES = gnsrun_gks.f
gnsrun_gks_LDADD = libgns.la `psx_link` `ems_link` `chr_link` `gks_link` `gwm_link`

gns_test_SOURCES = gns_test.f
gns_test_LDADD = libgns.la `psx_link` `ems_link` `chr_link` `gks_link` `gwm_link`
