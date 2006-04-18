## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = trn_link trn_link_adam

lib_LTLIBRARIES = libtrn.la

libtrn_la_SOURCES = \
	$(include_headers) \
	$(include_MESSAGES) \
	$(F_ROUTINES) \
	$(PRIVATE_INCLUDES)

include_HEADERS = TRN_PAR
include_MESSAGES = TRN_ERR

PRIVATE_INCLUDES = TRN_CMN TRN_CMN_SYM TRN_CONST TRN_CONST_STM

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

F_ROUTINES = trn1_symdt.f trn1_alwrk.f trn1_anlst.f trn1_antmp.f    \
trn1_cbcli.f trn1_cbcls.f trn1_clexp.f trn1_clvar.f trn1_cmpex.f    \
trn1_cmpfn.f trn1_cmpsl.f trn1_cmptm.f trn1_cmptr.f trn1_cpyst.f    \
trn1_cswap.f trn1_empst.f trn1_errc.f trn1_errl.f trn1_error.f	    \
trn1_evexd.f trn1_evexi.f trn1_evexr.f trn1_evsrt.f trn1_evtmd.f    \
trn1_evtmi.f trn1_evtmr.f trn1_excml.f trn1_exctt.f trn1_getsl.f    \
trn1_gtclc.f trn1_gtnv.f trn1_gtnvl.f trn1_gtnvs.f trn1_impid.f	    \
trn1_initl.f trn1_invcm.f trn1_invtm.f trn1_isnam.f trn1_movst.f    \
trn1_nvcls.f trn1_pscon.f trn1_psvar.f trn1_rdcls.f trn1_rddst.f    \
trn1_rdlog.f trn1_rdver.f trn1_relsl.f trn1_relts.f trn1_setup.f    \
trn1_temp.f trn1_trbtd.f trn1_trbti.f trn1_trbtr.f trn1_trdtd.f	    \
trn1_trdti.f trn1_trdtr.f trn1_upvsn.f trn1_valcl.f trn1_vfunc.f    \
trn1_vprec.f trn1_vsymb.f trn1_vtok.f trn1_vtr.f trn1_wrcls.f	    \
trn1_wrdst.f trn1_wrlog.f trn_annul.f trn_apnd.f trn_close.f	    \
trn_comp.f trn_gtcl.f trn_gtclc.f trn_gtnv.f trn_gtnvc.f trn_inv.f  \
trn_join.f trn_new.f trn_prfx.f trn_ptcl.f trn_stok.f trn_stokd.f   \
trn_stoki.f trn_stokr.f trn_tr1d.f trn_tr1i.f trn_tr1r.f trn_tr2d.f \
trn_tr2i.f trn_tr2r.f trn_trnd.f trn_trni.f trn_trnr.f

BUILT_SOURCES = $(include_MESSAGES)


# Testing

TESTS = trn_test
check_PROGRAMS = trn_test

trn_test_SOURCES = trn_test.f
# LDADD is the expansion of ref_link, with -lref replaced by libref.la
trn_test_LDADD = libtrn.la `hds_link` `err_link` `chr_link` `prm_link`

dist_starnews_DATA = trn.news
dist_pkgdata_DATA = LICENCE
