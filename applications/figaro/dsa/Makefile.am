## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

lib_LTLIBRARIES = libdsa.la

libdsa_la_SOURCES = \
        $(PRIVATE_INCLUDES) \
        $(BLOCK_DATA_FILES) \
        $(F_ROUTINES) \
	$(FPP_ROUTINES)

PRIVATE_INCLUDES = DSA_COMMON DSA_ERRORS DSA_STRUCTURE

BLOCK_DATA_FILES = dsa_block.f

F_ROUTINES = \
dyn_element.f dyn_increm.f \
 \
dsa__ary.f dsa__ary_ori.f dsa__axi_dat.f dsa__axi_la.f dsa__axi_lfl.f \
dsa__axi_nm.f dsa__axi_un.f dsa__axi_wi.f dsa__cl_de.f dsa__cr_ary.f \
dsa__cr_axi.f dsa__cr_axx.f dsa__cr_de.f dsa__cr_dx.f dsa__cr_ext.f \
dsa__cr_im.f dsa__cr_obj.f dsa__cr_obs.f dsa__cr_ql.f dsa__dat_env.f \
dsa__dat_nm.f dsa__del_axi.f dsa__del_im.f dsa__del_ql.f dsa__err_nm.f \
dsa__exp_nm.f dsa__ext_nm.f dsa__flush.f dsa__fnd_ndf.f dsa__fts_dtl.f \
dsa__fts_spc.f dsa__gt_bb.f dsa__im_nm.f dsa__inv_rg.f dsa__la_nm.f \
dsa__mag_fl.f dsa__ndf_top.f dsa__nth_dat.f dsa__nth_ndf.f dsa__obj_nm.f \
dsa__parse.f dsa__pr_gt.f dsa__pr_pt.f dsa__ql_nm.f dsa__rd_fts.f \
dsa__rd_ndf.f dsa__rg_sc.f dsa__sc_ary.f dsa__secz_nm.f dsa__sk_fl.f \
dsa__sm_form.f dsa__st_fl.f dsa__st_form.f dsa__st_ty.f dsa__sv_axi.f \
dsa__top_ty.f dsa__ty_dtl.f dsa__un_nm.f dsa__var_nm.f dsa__wr_fts.f \
 \
dsa_ac_mp_ql.f dsa_actname.f dsa_add_sr.f dsa_ary_dfnd.f dsa_ary_exs.f \
dsa_ary_sz.f dsa_ary_ty.f dsa_axi_rg.f dsa_axi_sz.f dsa_axi_ty.f \
dsa_axi_wi.f dsa_build_sr.f dsa_calc_wi.f dsa_cfill.f dsa_cfillx.f \
dsa_ch_bb.f dsa_ch_mp.f dsa_ch_ndf.f dsa_ch_sr.f dsa_close.f \
dsa_close_sr.f dsa_co_ary.f dsa_co_axi.f dsa_co_dat.f dsa_compaf.f \
dsa_cr_ary.f dsa_cr_nm_sr.f dsa_cr_sr.f dsa_cstruct.f dsa_cstrx.f \
dsa_dat_loc.f dsa_dat_sz.f dsa_dat_ty.f dsa_dec_dims.f dsa_def_sc.f \
dsa_defext.f dsa_defexts.f dsa_del_axi.f dsa_del_im.f dsa_del_ql.f \
dsa_dump_cmn.f dsa_element.f dsa_encdim.f dsa_err_in.f dsa_err_ty.f \
dsa_err_var.f dsa_fill_wi.f dsa_fl_x.f dsa_fmtcon.f dsa_fname.f \
dsa_fnd_ref.f dsa_free_lu.f dsa_free_wk.f dsa_fts_buff.f \
dsa_gt_ac_nm.f dsa_gt_air.f dsa_gt_axi.f dsa_gt_dat.f dsa_gt_exp.f \
dsa_gt_fl.f dsa_gt_fts.f dsa_gt_fts_x.f dsa_gt_lu.f dsa_gt_rg.f \
dsa_gt_wk.f dsa_gt_wkary.f dsa_im_ty.f dsa_init_ref.f dsa_input.f \
dsa_main_sz.f dsa_mp_ary.f dsa_mp_axi.f dsa_mp_dat.f dsa_mp_dummy.f \
dsa_mp_ers.f dsa_mp_im.f dsa_mp_ql.f dsa_mp_some.f dsa_mp_var.f \
dsa_mp_wi.f dsa_mt_axes.f dsa_mt_axi.f dsa_mt_dim.f dsa_mt_sz.f \
dsa_mt_un.f dsa_nfill.f dsa_nfillx.f dsa_nm_in.f dsa_nm_out.f \
dsa_nth_fts.f dsa_obj_nm.f dsa_open.f dsa_open_tf.f dsa_output.f \
dsa_pr_pc_fl.f dsa_pr_pc_ql.f dsa_pref_ty.f dsa_ps_gfts.f \
dsa_ps_pc_fl.f dsa_ps_pc_ql.f dsa_ps_pfts.f dsa_pt_fts_x.f \
dsa_qf_ch.f dsa_ql_fl_ok.f dsa_ql_ty.f dsa_rd_sc.f dsa_real_nm.f \
dsa_ref_slot.f dsa_refl_dat.f dsa_refl_x.f dsa_rename_t.f \
dsa_rest_axi.f dsa_rs_ary.f dsa_rs_axi.f dsa_rs_dat.f dsa_setobj.f \
dsa_sk_axi.f dsa_sk_ers.f dsa_sk_fl.f dsa_sk_fts.f dsa_sk_im.f \
dsa_sk_nm_sr.f dsa_sk_ql.f dsa_sk_rg.f dsa_sk_sr.f dsa_sk_var.f \
dsa_sk_wi.f dsa_sm_dat.f dsa_smpl_out.f dsa_spec_sr.f dsa_sqrt_x.f \
dsa_square_x.f dsa_st_axi.f dsa_st_dat.f dsa_st_exp.f dsa_st_fl.f \
dsa_st_obj.f dsa_st_rg.f dsa_st_sc.f dsa_st_wi.f dsa_status.f \
dsa_structc.f dsa_sv_axi.f dsa_trnl_var.f dsa_typesize.f \
dsa_unfl_dat.f dsa_unfl_x.f dsa_unmap.f dsa_use_fl.f dsa_use_ql.f \
dsa_val_axi.f dsa_var_err.f dsa_var_ty.f dsa_wrflush.f dsa_wrname.f \
dsa_wruser.f dsa_xcstr.f dsa_zfill.f \
dsaz_fname.f dsaz_inq_nam.f

FPP_ROUTINES = dsaz_tfopen.F
