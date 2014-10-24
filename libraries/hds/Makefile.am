## Process this file with automake to produce Makefile.in

bin_PROGRAMS = hdsdump
dist_bin_SCRIPTS = hds_link hds_link_adam
noinst_PROGRAMS = hds_machine dat_par_h dat_par_f make-hds-types
starhelp_DATA = hds_machine.txt

# libhdsf is for fortran interface
lib_LTLIBRARIES = libhds.la libhdsf.la

TESTS = hds_test hdsTest hds_test_prm
check_PROGRAMS = hds_test hdsTest hds_test_prm

libhds_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PUBLIC_CINCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES) \
	$(C_ROUTINES)

libhds_la_LIBADD = `starmem_link` `ems_link`

libhdsf_la_SOURCES = \
	$(F_ROUTINES) \
	$(F_C_ROUTINES) \
	$(BLOCK_DATA_FILES)

libhdsf_la_LIBADD = `ems_link` `one_link`

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libhds_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libhds_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libhdsf_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libhdsf_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
endif

hds_machine_SOURCES = hds_machine.c
hds_machine_LDADD = libhds.la `ems_link` `cnf_link` `starmem_link`

dat_par_h_SOURCES = dat_par_h.c
dat_par_f_SOURCES = dat_par_f.c
make_hds_types_SOURCES = make-hds-types.c

hdsdump_SOURCES = hds_dev/hdsdump.c
hdsdump_LDADD = libhds.la `ems_link` `cnf_link` `starmem_link`

# Try to make sure that hds_types.h is built early
# This does not seem to work right in conjunction with BUILT_SOURCES
dat_par_h_DEPENDENCIES = hds_types.h hds1_types.h
dat_par_f_DEPENDENCIES = hds_types.h hds1_types.h

hds_test_SOURCES = hds_test.f
hds_test_LDADD = libhdsf.la libhds.la `ems_link` `chr_link` `cnf_link`

hdsTest_SOURCES = hdsTest.c
hdsTest_LDADD = libhds.la `ems_link` `cnf_link` `cnf_link`

hds_test_prm_SOURCES = hds_test_prm.c
hds_test_prm_LDADD = libhds.la `ems_link` `cnf_link` `cnf_link`

cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_CINCLUDES)
include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = CMP_ERR DAT_ERR dat_err.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# dat_par.h was previously released so must go in the root include dir
PUBLIC_CINCLUDES = hds.h hds_types.h hds_fortran.h cmp.h
PUBLIC_INCLUDES = DAT_PAR dat_par.h

PRIVATE_INCLUDES = \
	CMP_CCT CMP_CONST DAT1_SYS \
	dat1.h hds1.h hds2.h hds1_types.h \
	rec.h rec1.h str.h \
        win_fixups.h \
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

CMP_C_ROUTINES = cmpMod.c cmpPut0x.c cmpType.c

C_ROUTINES = $(CMP_C_ROUTINES) \
	dat1_alloc_lcp.c dat1_annul_lcp.c dat1_check_mode.c \
	dat1_check_type.c dat1_cvt_format.c dat1_cvt_order.c \
	dat1_decoy.c dat1_get_ncomp.c dat1_get_odl.c dat1_getenv.c \
	dat1_import_loc.c dat1_init.c dat1_init_ndr.c dat1_intune.c \
	dat1_locate_name.c dat1_pack_crv.c dat1_pack_odl.c \
	dat1_pack_srv.c dat1_put_ncomp.c dat1_put_odl.c \
	dat1_show_ndr.c dat1_unpack_crv.c dat1_unpack_odl.c \
	dat1_unpack_srv.c dat1_unpack_type.c \
        dat1_free_hdsloc.c dat1_import_floc.c \
	datClen.c datCopy.c datMsg.c datRef.c \
	datDrep.c datErmsg.c datParen.c datPrec.c datPrmry.c datRefct.c \
	datWhere.c datalter.c datannul.c datccopy.c daterase.c datfinind.c \
	datget.c datget0x.c datinq.c datlocops.c datmap.c datmove.c datnew.c \
	datnew0.c datMapN.c datPut1C.c datPutVC.c datGet1C.c datGetVC.c \
	datChscn.c \
	datput.c datput0x.c dattemp.c daucheck.c daucnv.c daucopy.c \
        datLocked.c datLock.c datUnlock.c datNolock.c \
	dauflush.c \
	dauloc.c daumove.c dauscatgath.c daushape.c dautypes.c \
	datExportFloc.c	datImportFloc.c hdsdim.c hds1_cleanup.c \
	hds1_exit.c hdsCopy.c hdsEwild.c hds_globals.c hdsGtune.c \
	hdsState.c hdsStop.c hdsTune.c \
	hdsWild.c hdserase.c hdsgroup.c hdslock.c hdsnew.c hdsnull.c \
	hdsopclos.c hdstools.c  hdsInfoI.c \
	dat1emsSetBigi.c dat1emsSetBigu.c dat1emsSetHdsdim.c \
	rec1_alloc_frame.c rec1_clear_cbm.c rec1_close_file.c \
	rec1_close_slot.c rec1_create_file.c rec1_deall_frame.c \
	rec1_extend_file.c rec1_extend_frame.c rec1_find_file.c \
	rec1_flush_block.c rec1_fmsg.c rec1_get_fid.c rec1_get_path.c \
	rec1_getcwd.c rec1_locate_hcb.c rec1_lock_slot.c \
	rec1_map_frame.c rec1_open_file.c rec1_pack_chain.c \
	rec1_pack_hcb.c rec1_pack_ncomp.c rec1_pack_rcl.c \
	rec1_read_file.c rec1_save_addr.c rec1_scan_cbm.c \
	rec1_set_cbm.c rec1_shell.c rec1_test_cbm.c rec1_unlock_slot.c \
	rec1_unmap_frame.c rec1_unpack_chain.c rec1_unpack_hcb.c \
	rec1_unpack_ncomp.c rec1_unpack_rcl.c rec1_update_free.c \
	rec1_write_file.c \
	\
	rec_adopt_record.c rec_alloc_mem.c rec_alloc_xmem.c \
	rec_attach_file.c rec_close_file.c rec_create_record.c \
	rec_count_files.c \
	rec_deall_mem.c rec_deall_xmem.c rec_delete_record.c \
	rec_end_wild.c rec_extend_record.c rec_get_handle.c \
	rec_get_rcl.c rec_get_rid.c rec_list_files.c \
	rec_locate_block.c rec_locate_data.c rec_locate_fns.c \
	rec_lock.c rec_mark_delete.c rec_reall_mem.c rec_refcnt.c \
	rec_release_block.c rec_release_data.c rec_reset_record.c \
	rec_same_file.c rec_shrink_record.c rec_start.c rec_stop.c \
	rec_unlock.c rec_where.c rec_wild_file.c \
	rtl_fixups.c \
        win_fixups.c \
	hds_start.c

dat_par.h: dat_par_h$(EXEEXT)
	./dat_par_h >dat_par.h

DAT_PAR: dat_par_f$(EXEEXT)
	./dat_par_f > DAT_PAR

hds_types.h: make-hds-types$(EXEEXT)
	./make-hds-types

hds1_types.h: make-hds-types$(EXEEXT)
	./make-hds-types

hds_machine.txt: hds_machine$(EXEEXT)
	echo "Machine characteristics, as determined by HDS" >$@
	echo "" >>$@
	./hds_machine >>$@

# Have to make sure that hds_types appears before DAT_PAR so that
# the include file generation routines are built in the correct order
BUILT_SOURCES = hds_types.h hds1_types.h \
	$(include_MESSAGES) $(noinst_MESSAGES) DAT_PAR dat_par.h

CLEANFILES = DAT_PAR dat_par.h hds_machine.txt CMP_ERR DAT_ERR dat_err.h \
	hds_types.h hds1_types.h

dist_starnews_DATA = hds.news

EXTRA_DIST = cmp_err.msg dat_err.msg fortran_interface.h

