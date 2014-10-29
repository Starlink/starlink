## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = hds_link hds_link_adam
noinst_PROGRAMS = dat_par_f make-hds-types

# libhdsf is for fortran interface
lib_LTLIBRARIES = libhds.la libhdsf.la

TESTS = hds_test hdsTest
check_PROGRAMS = hds_test hdsTest

libhds_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PUBLIC_CINCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES) \
	$(C_ROUTINES)

libhds_la_LIBADD = -lhds_v4 -lhds_v5

libhdsf_la_SOURCES = \
	$(F_ROUTINES) \
	$(F_C_ROUTINES) \
	$(BLOCK_DATA_FILES)

libhdsf_la_LIBADD = libhds.la `ems_link` `one_link` `chr_link`

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libhds_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libhds_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libhdsf_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libhdsf_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
endif

dat_par_f_SOURCES = dat_par_f.c
make_hds_types_SOURCES = make-hds-types.c

# Try to make sure that hds_types.h is built early
# This does not seem to work right in conjunction with BUILT_SOURCES
dat_par_f_DEPENDENCIES = hds_types.h hds1_types.h dat_par.h

hds_test_SOURCES = hds_test.f
hds_test_LDADD = libhdsf.la

hdsTest_SOURCES = hdsTest.c
hdsTest_LDADD = libhds.la

cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_CINCLUDES)
include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = CMP_ERR DAT_ERR dat_err.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# dat_par.h was previously released so must go in the root include dir
PUBLIC_CINCLUDES = hds.h hds_types.h hds_fortran.h cmp.h
PUBLIC_INCLUDES = DAT_PAR dat_par.h

PRIVATE_INCLUDES = \
	CMP_CCT CMP_CONST DAT1_SYS dat1.h \
	$(noinst_MESSAGES)

F_ROUTINES = \
	cmp_activ.f cmp_deact.f cmp_erdsn.f cmp_get0x.f cmp_get1x.f \
	cmp_getnx.f cmp_getvx.f cmp_len.f cmp_mapn.f cmp_mapv.f \
	cmp_mod.f cmp_modc.f cmp_prim.f cmp_put0x.f cmp_put1x.f \
	cmp_putnx.f cmp_putvx.f cmp_shape.f cmp_size.f cmp_struc.f \
	cmp_type.f cmp_unmap.f \
	dat_getnx.f dat_putnx.f \
	dat_cut.f hds_split.f \
	dat1_pshde.f dat1_pshdf.f dat1_pshdb.f \
	hds_find.f

F_C_ROUTINES = hds_run.c fortran_interface.c hdsFind.c

#  List of Fortran BLOCK DATA routines.
BLOCK_DATA_FILES = cmp_blk.f

C_ROUTINES = \
	datExportFloc.c datImportFloc.c dat1_import_floc.c hdsdim.c \
	hds_select.c datCopy5to4.c datCopy4to5.c \
	datCcopy5to4.c datCcopy4to5.c

DAT_PAR: dat_par_f$(EXEEXT)
	./dat_par_f > DAT_PAR

hds_types.h: make-hds-types$(EXEEXT)
	./make-hds-types

hds1_types.h: make-hds-types$(EXEEXT)
	./make-hds-types

# Have to make sure that hds_types appears before DAT_PAR so that
# the include file generation routines are built in the correct order
BUILT_SOURCES = hds_types.h hds1_types.h \
	$(include_MESSAGES) $(noinst_MESSAGES) DAT_PAR dat_par.h

CLEANFILES = DAT_PAR dat_par.h hds_machine.txt CMP_ERR DAT_ERR dat_err.h \
	hds_types.h hds1_types.h

dist_starnews_DATA = hds.news

EXTRA_DIST = cmp_err.msg dat_err.msg fortran_interface.h

