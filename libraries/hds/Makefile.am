## Process this file with automake to produce Makefile.in

bin_SCRIPTS = hds_link hds_link_adam
noinst_PROGRAMS = hds_machine dat_par_h
starhelp_DATA = hds_machine.txt

lib_LTLIBRARIES = libhds.la

TESTS = hds_test hds_test_prm
check_PROGRAMS = hds_test hds_test_prm

libhds_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(C_ROUTINES) \
	$(F_ROUTINES) \
	$(BLOCK_DATA_FILES)

hds_machine_SOURCES = hds_machine.f
hds_machine_LDADD = libhds.la `ems_link` `chr_link` `cnf_link`

dat_par_h_SOURCES = dat_par_h.c

hds_test_SOURCES = hds_test.f
hds_test_LDADD = libhds.la `ems_link` `chr_link`

hds_test_prm_SOURCES = hds_test_prm.c
hds_test_prm_LDADD = libhds.la `ems_link` `chr_link` `cnf_link`

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = CMP_ERR DAT_ERR DAT_PAR dat_par.h
PRIVATE_INCLUDES = \
	CMP_CCT CMP_CONST DAT1_SYS \
	dat1.h dat_err.h hds1.h \
	hds1_feature.h rec.h rec1.h str.h

F_ROUTINES = \
	cmp_activ.f cmp_deact.f cmp_erdsn.f cmp_get0x.f cmp_get1x.f \
	cmp_getnx.f cmp_getvx.f cmp_len.f cmp_mapn.f cmp_mapv.f \
	cmp_mod.f cmp_modc.f cmp_prim.f cmp_put0x.f cmp_put1x.f \
	cmp_putnx.f cmp_putvx.f cmp_shape.f cmp_size.f cmp_struc.f \
	cmp_type.f cmp_unmap.f dat_ccopy.f dat_cctyp.f dat_erdsc.f \
	dat_erdsn.f dat_ertxt.f dat_get0x.f dat_get1x.f dat_getnx.f \
	dat_getvx.f dat_mapn.f dat_mapv.f dat_msg.f dat_new0.f \
	dat_new0c.f dat_new0x.f dat_new1.f dat_new1c.f dat_new1x.f \
	dat_newc.f dat_prec.f dat_put0x.f dat_put1x.f dat_putnx.f \
	dat_putvx.f dat_rcera.f dat_rcopy.f dat_ref.f dat_tune.f

#  List of Fortran BLOCK DATA routines.
BLOCK_DATA_FILES = cmp_blk.f

C_ROUTINES = \
	dat1_alloc_lcp.c dat1_annul_lcp.c dat1_check_mode.c \
	dat1_check_type.c dat1_cvt_format.c dat1_cvt_order.c \
	dat1_decoy.c dat1_get_ncomp.c dat1_get_odl.c dat1_getenv.c \
	dat1_import_loc.c dat1_init.c dat1_init_ndr.c dat1_intune.c \
	dat1_locate_name.c dat1_pack_crv.c dat1_pack_odl.c \
	dat1_pack_srv.c dat1_put_ncomp.c dat1_put_odl.c \
	dat1_show_ndr.c dat1_unpack_crv.c dat1_unpack_odl.c \
	dat1_unpack_srv.c dat1_unpack_type.c dat_clen.c dat_copy.c \
	dat_drep.c dat_ermsg.c dat_paren.c dat_prmry.c dat_refct.c \
	dat_where.c datalter.c datannul.c daterase.c datfinind.c \
	datget.c datinq.c datlocops.c datmap.c datmove.c datnew.c \
	datput.c dattemp.c daucheck.c daucnv.c daucopy.c dauflush.c \
	dauloc.c daumove.c dauscatgath.c daushape.c dautypes.c \
	\
	hds1_exit.c hds_copy.c hds_ewild.c hds_globals.c hds_gtune.c \
	hds_run.c hds_start.c hds_state.c hds_stop.c hds_tune.c \
	hds_wild.c hdserase.c hdsgroup.c hdslock.c hdsnew.c hdsnull.c \
	hdsopclos.c hdstools.c  \
	\
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
	rec_deall_mem.c rec_deall_xmem.c rec_delete_record.c \
	rec_end_wild.c rec_extend_record.c rec_get_handle.c \
	rec_get_rcl.c rec_get_rid.c rec_list_files.c \
	rec_locate_block.c rec_locate_data.c rec_locate_fns.c \
	rec_lock.c rec_mark_delete.c rec_reall_mem.c rec_refcnt.c \
	rec_release_block.c rec_release_data.c rec_reset_record.c \
	rec_same_file.c rec_shrink_record.c rec_start.c rec_stop.c \
	rec_unlock.c rec_where.c rec_wild_file.c \
	\
	rtl_fixups.c

dat_par.h: dat_par_h
	./dat_par_h >dat_par.h

hds_machine.txt: hds_machine
	echo "Machine characteristics, as determined by HDS" >$@
	echo "" >>$@
	./hds_machine >>$@

BUILT_SOURCES = DAT_ERR dat_err.h dat_par.h CMP_ERR
