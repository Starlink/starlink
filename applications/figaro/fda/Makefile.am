## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

lib_LTLIBRARIES = libfda.la

libfda_la_SOURCES = \
        $(PRIVATE_INCLUDES) \
        $(BLOCK_DATA_FILES) \
        $(F_ROUTINES) $(FPP_ROUTINES)

PRIVATE_INCLUDES = DSA_COMMON DSA_STRUCTURE DSA_TYPES

BLOCK_DATA_FILES = dsa_block.f

F_ROUTINES = \
dyn_incad.f dyn_increm.f dsa_open.f dsa_close.f \
dsa_dump_cmn.f dsa_input.f dsa_nm_in.f dsa_inputu.f dsa_nm_inu.f \
dsa_output.f dsa_nm_out.f  dsa_smpl_out.f  dsa_close_sr.f dsa_unmap.f \
dsa_gt_ac_nm.f dsa_use_fl.f dsa_use_ql.f dsa_ql_fl_ok.f \
dsa_sk_fl.f dsa_sk_ql.f dsa_gt_fl.f dsa_st_fl.f dsa_dat_sz.f \
dsa_dat_ty.f dsa_gt_dat.f dsa_st_dat.f dsa_sk_rg.f dsa_st_rg.f \
dsa_gt_rg.f dsa_mp_dat.f dsa_mp_ql.f dsa_del_ql.f dsa_sk_var.f \
dsa_sk_ers.f dsa_err_in.f dsa_mp_var.f dsa_mp_ers.f dsa_sk_im.f \
dsa_del_im.f dsa_mp_cmplx.f dsa_co_dat.f dsa_rs_dat.f \
dsa_gt_air.f dsa_gt_exp.f dsa_obj_nm.f dsa_st_obj.f dsa_sk_axi.f \
dsa_axi_sz.f dsa_mp_axi.f dsa_axi_rg.f dsa_del_axi.f dsa_rs_axi.f \
dsa_co_axi.f dsa_gt_axi.f dsa_st_axi.f dsa_sk_wi.f dsa_mp_wi.f \
dsa_st_wi.f dsa_mt_sz.f dsa_sm_dat.f dsa_mt_axi.f dsa_mt_axes.f \
dsa_mt_dim.f dsa_sk_fts.f dsa_nth_fts.f dsa_gt_fts_x.f dsa_pt_fts_x.f \
dsa_gt_lu.f dsa_free_lu.f dsa_open_tf.f dsa_typesize.f dsa_gt_wk.f \
dsa_gt_wkary.f dsa_free_wk.f dsa_fmtcon.f dsa_encdim.f dsa_pref_ty.f \
dsa_wruser.f dsa_spec_sr.f dsa_rd_sc.f dsa_element.f dsa_st_sc.f \
dsa_setobj.f dsa_add_sr.f dsa_cr_sr.f dsa_cr_nm_sr.f dsa_pt_hist.f \
dsa1_chkstr.f dsa1_rdnam.f dsa1_rfnd.f dsa1_msnew.f dsa1_ndftyp.f \
dsa1_mdat.f dsa1_mdf.f dsa1_mdq.f dsa1_mdqf.f dsa1_mrgfq.f dsa1_clean.f \
dsa1_axisnd.f dsa1_cpdat.f dsa1_tfopen.f dsa2_nfillf.f dsa3_build.f \
 \
dta_crnam.f dta_crvar.f dta_dlvar.f dta_szvar.f dta_struc.f \
dta_rdvarx.f dta_wrvarx.f dta_loc.f dta_annul.f dta1_there.f \
dta1_split.f dta1_decdim.f dsa2_acre.f dsa2_afillf.f dsa2_ifillf.f \
dsa2_ifilld.f dsa2_pixin.f

FPP_ROUTINES = dyn_element.F
