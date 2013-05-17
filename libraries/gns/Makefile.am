## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = gns_link gns_link_adam

lib_LTLIBRARIES = libgns.la

libgns_la_SOURCES = \
  $(F_ROUTINES) \
  $(FPP_ROUTINES) \
  $(PUBLIC_INCLUDES) \
  $(include_MESSAGES) \
  $(PRIVATE_INCLUDES)

libgns_la_LIBADD = `ems_link`

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = GNS_ERR

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# These etc files are binary, so should not go into share
staretc_DATA = gns_gksdevices gns_gksnames gns_ididevices gns_idinames

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it.

PUBLIC_INCLUDES = GNS_PAR

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

FPP_ROUTINES = initg.F initi.F

BUILT_SOURCES = GNS_ERR

# Need to specify how to create the binary database files

noinst_PROGRAMS = gksbuild idibuild
gksbuild_SOURCES = gksbuild.F
idibuild_SOURCES = idibuild.F

gns_gksdevices : gksbuild$(EXEEXT) gksdevices.txt
	-rm -f gns_gksdevices
	echo gksdevices.txt | ./gksbuild
	chmod o+r gns_gksdevices

gns_ididevices : idibuild$(EXEEXT) ididevices.txt
	-rm -f gns_ididevices
	echo ididevices.txt | ./idibuild
	chmod o+r gns_ididevices

# Tests

TESTS = gns_test gnsrun_gks

check_PROGRAMS = gns_test gnsrun_gks

gnsrun_gks_SOURCES = gnsrun_gks.f
gnsrun_gks_LDADD = libgns.la `psx_link` `err_link` `chr_link` `gks_link` `gwm_link`

gns_test_SOURCES = gns_test.f
gns_test_LDADD = libgns.la `psx_link` `err_link` `chr_link` `gks_link` `gwm_link`

CLEANFILES = gns_ididevices gns_gksdevices GNS_ERR

dist_starnews_DATA = gns.news
dist_pkgdata_DATA = LICENCE gnsrun_gks.f

EXTRA_DIST = gns_idinames ididevices.txt gns_gksnames gksdevices.txt gns_err.msg





