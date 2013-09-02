## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = sgs_link

lib_LTLIBRARIES = libsgs.la

libsgs_la_SOURCES = $(F_ROUTINES) $(include_MESSAGES) \
	$(PRIVATE_INCLUDES)

libsgs_la_LIBADD = `gns_link`

include_MESSAGES = SGS_ERR

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sun85_diag1.pdf

#  There are no other public include files.

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = sgscom

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES =  \
apoly.f arc.f atext.f atxb.f atxi.f atxl.f atxr.f bnorm.f box.f bpoly.f \
btext.f bzndc.f circl.f close.f clrbl.f clrfg.f clrz.f clswk.f clwk.f \
cuvis.f defch.f discu.f enscu.f err.f flush.f getz.f gkerr.f gksin.f \
gkstm.f hstat.f ichmo.f icuav.f icurw.f icurz.f idun.f ilcmo.f imtx.f \
incho.f init.f ipen.f iplxy.f isler.f istat.f itxa.f itxb.f izone.f \
line.f mark.f markl.f newz.f norm.f open.f opnwk.f opoly.f otext.f \
pnz.f relz.f reqch.f reqcu.f sartx.f selch.f selz.f setcu.f \
settx.f sfont.f sgsin.f shmk.f shtx.f spen.f sprec.f \
ssptx.f stxj.f suptx.f sw.f tpz.f tx.f txi.f txr.f upcas.f wdtrd.f \
widen.f wlist.f wlst.f wname.f zone.f zpart.f zshap.f \
zsize.f

BUILT_SOURCES = SGS_ERR

# Tests. These are normally installed into the examples directory

TESTS = sgs_test sgs_workstations sgsx1 sgsx2

check_PROGRAMS = sgs_test sgs_workstations sgsx1 sgsx2

sgs_workstations_SOURCES = sgs_workstations.f
sgs_workstations_LDADD = libsgs.la `gns_link` `gks_link` `ems_link` `chr_link`

sgs_test_SOURCES = sgs_test.f
sgs_test_LDADD = libsgs.la `gns_link` `gks_link` `ems_link` `chr_link`

sgsx1_SOURCES = sgsx1.f
sgsx1_LDADD = libsgs.la `gns_link` `gks_link` `ems_link` `chr_link`

sgsx2_SOURCES = sgsx2.f
sgsx2_LDADD = libsgs.la `gns_link` `gks_link` `ems_link` `chr_link`

dist_starnews_DATA = sgs.news
dist_pkgdata_DATA = LICENCE sgsx1.f sgsx2.f sgs_workstations.f
