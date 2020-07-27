## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = cat_link cat_link_adam

lib_LTLIBRARIES = libcat.la libcat_adam.la

libcat_la_SOURCES = \
	$(BLOCK_DATA_FILES) \
	$(F_ROUTINES) \
	$(GEN_F_ROUTINES)

libcat_la_DEPENDENCIES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES)

libcat_adam_la_SOURCES = \
	$(ADAM_F_ROUTINES)

libcat_adam_la_DEPENDENCIES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES)


## Need dependency to get include files generated.
$(libcat_la_SOURCES): $(PUBLIC_INCLUDES)
$(libcat_adam_la_SOURCES): $(PUBLIC_INCLUDES)

## List headers which are to be installed -- these are built first if
## necessary, XXX oh no they are not! Hence above.
include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = CAT_ERR

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

PUBLIC_INCLUDES = CAT_PAR

PRIVATE_INCLUDES = CAT1_NUL CAT1_PAR CAT1_AST_CMN CAT1_ATTRB_CMN \
    CAT1_CATS_CMN   CAT1_CTRL_CMN   CAT1_DSDIR_CMN  CAT1_IDS_CMN \
    CAT3_FIT_PAR CAT3_FIT_CMN CAT5_ANG_CMN CAT5_STL_CMN \
    CAT6_TST_CMN antx1.inc antx2.inc antx3.inc

F_ROUTINES =   cat1_dceqp.f  cat1_tidnt.f  cat5_wrpar.f               \
 ant_cfind.f   cat1_dcitm.f  cat1_tidtp.f  cat5_wrtbl.f  cat_fgt0f.f  \
 ant_err.f     cat1_topen.f  cat6_cknme.f  \
 ant_errc.f    cat1_dcnme.f  cat3_close.f  cat6_close.f  cat_getxt.f  \
 ant_errd.f    cat1_dcrcd.f  cat3_cnew.f   cat6_cnew.f   cat_iinq.f  \
 ant_erri.f    cat1_dctyp.f  cat3_fgcvt.f  cat6_coldt.f  cat_inew.f  \
 cat1_ddscr.f  cat3_fincr.f  cat6_crcol.f  cat_pinq.f  \
 ant_s.f       cat1_dfatt.f  cat6_crpar.f  cat_pnew0.f  \
 ant_sevalt.f  cat1_dmrcd.f  cat3_fiter.f  cat6_ddscr.f \
 ant_x.f       cat1_dpatt.f  cat3_getxt.f  cat6_dtype.f \
 cat1_dxfmt.f  cat3_gtcla.f  cat6_eprse.f  \
 cat0_close.f  cat3_gtclb.f  cat6_fincr.f  cat_putxt.f  \
 cat0_cnew.f   cat1_ecscr.f  cat3_gtpar.f  cat_rapnd.f  \
 cat0_fincr.f  cat1_edscr.f  cat3_gtxcl.f  cat6_getxt.f  cat_rget.f  \
 cat1_eqckl.f  cat_rset.f  \
 cat0_getxt.f  cat1_errep.f  cat3_opnex.f  cat6_gtcae.f  cat_rstxt.f  \
 cat0_opnex.f  cat1_errtr.f  cat3_putxt.f  cat6_gtcld.f  cat_selct.f  \
 cat0_putxt.f  cat1_fincr.f  cat6_opnex.f  \
 cat0_rstxt.f  cat3_rdord.f  cat6_putxt.f  cat_sinq.f  \
 cat1_frear.f  cat3_rstxt.f  cat6_rdrow.f  cat_slist.f  \
 cat1_addcl.f  cat1_fresp.f  cat3_wtord.f  cat6_rdscr.f  \
 cat1_addpr.f  cat1_getlu.f  cat5_ancdc.f  cat6_rdtbl.f  cat_sztxt.f  \
 cat1_anew.f   cat1_gnsrt.f  cat5_close.f  cat6_rstxt.f  \
 cat1_anfmt.f  cat1_gtdat.f  cat5_cnew.f   cat6_spcol.f  cat_tcols.f  \
 cat1_angdc.f  cat1_gtsra.f  cat5_cntrc.f  cat6_split.f  cat_tdetl.f  \
 cat1_anprs.f  cat1_gtsrw.f  cat5_dcang.f  cat_tidnt.f  \
 cat1_appnd.f  cat1_ioerr.f  cat5_fincr.f  cat6_stcae.f  cat_tidpr.f  \
 cat1_axset.f  cat1_msg.f    cat6_stnul.f  cat_tidtp.f  \
 cat5_getxt.f  cat6_wrcol.f  \
 cat1_catra.f  cat1_nmchk.f  cat5_glfmt.f  cat6_wrpar.f  cat_tndnt.f  \
 cat1_catrw.f  cat1_nmunq.f  cat6_wrtbl.f  cat_topen.f  \
 cat1_cielm.f  cat1_prsvn.f  cat5_gtcae.f  cat_aexst.f   cat_trlse.f  \
 cat1_ckmut.f  cat5_gtcld.f  cat_aget.f    cat_trows.f  \
 cat1_cnmpr.f  cat1_qsrtd.f  cat5_gtval.f  cat_aput.f    cat_tuneg.f  \
 cat1_cpyar.f  cat1_rclsp.f  cat5_opnex.f  cat_aset.f    cat_tunes.f  \
 cat1_crcol.f  cat1_rdscr.f  cat5_putxt.f                cat_tyfmt.f  \
 cat1_crpar.f  cat1_rjlst.f  cat5_rdtbl.f  cat_cinq.f    \
 cat1_crtar.f  cat1_rngls.f  cat5_rdtbx.f  cat_cnewa.f   \
 cat1_crtci.f  cat1_selct.f  cat5_rstxt.f  cat_cnews.f   \
 cat1_crtel.f  cat1_snkta.f                \
 cat1_crtid.f  cat1_srcta.f  cat5_stcae.f  cat_egt0f.f   \
 cat1_crtsl.f  cat1_stdir.f  cat5_stnul.f  \
 cat1_cxfmt.f  cat1_tcnvt.f  cat5_wrcol.f  cat_eidnt.f \
 cat1_fioer.f  cat_gcolc.f   cat_gcold.f   cat_gcolr.f

ADAM_F_ROUTINES = cat_assoc.f cat_creat.f cat_exist.f

GEN_F_ROUTINES = $(CAT_GEN_ROUTINES:.gen=.f) \
$(CAT_GCN_ROUTINES:.gcn=.f) \
$(CAT_GN_ROUTINES:.gn=.f)

CAT_GEN_ROUTINES = \
ant_fgetx.gen \
cat0_fiox.gen \
cat1_addax.gen \
cat1_bhopx.gen \
cat1_dcnlx.gen \
cat1_ecnlx.gen \
cat1_fiox.gen \
cat1_putx.gen \
cat3_fiox.gen \
cat3_gvalx.gen \
cat3_pvalx.gen \
cat5_fiox.gen \
cat5_gtaex.gen \
cat5_staex.gen \
cat6_fiox.gen \
cat6_gtaex.gen \
cat6_staex.gen \
cat_egt0x.gen \
cat_fgt0x.gen \
cat_pptax.gen \
cat_pptsx.gen \
cat_put0x.gen \
cat_sfndx.gen \
cat_srngx.gen \
cat_tattx.gen \
cat_tiqax.gen

# Numeric
CAT_GN_ROUTINES = \
ant_sevalx.gn

# All except LOGICAL
CAT_GCN_ROUTINES = \
cat1_eqckx.gcn \
cat1_nlckx.gcn

BLOCK_DATA_FILES = ant_block.f attrbb.f catsb.f ctrlb.f idsb.f

BUILT_SOURCES = $(include_MESSAGES) $(PRIVATE_INCLUDES)

EXAMPLE_FILES = examples/0README.LIS \
examples/car_choi1.f \
examples/car_choic.f \
examples/car_gtfld.f \
examples/car_gtlst.f \
examples/car_lxfmt.f \
examples/car_ptlst.f \
examples/cat-examples_link_adam \
examples/cat-examples_output.lis \
examples/cat-examples_test.csh \
examples/complex.TXT \
examples/complex.dat \
examples/details.f \
examples/details.ifl \
examples/example_angles.f \
examples/example_angles.ifl \
examples/example_read.f \
examples/example_read.ifl \
examples/example_write.f \
examples/example_write.ifl \
examples/kappa.TXT \
examples/listout.f \
examples/listout.ifl \
examples/poly.TXT \
examples/scale.TXT \
examples/simple.TXT \
examples/simple_radians.TXT


## Create CAT1_NUL from the processed source. Need to add include of PRM_PAR
## by hand to avoid expanding that.
CAT1_NUL: CAT1_NUL.F
	rm -f $@
	$(FPP) $(FPPFLAGS) $(FPP_MAKE_FLAGS) CAT1_NUL.F $(FPP_OUTPUT)
	test -f $@ || mv CAT1_NUL.f $@
	rm -f CAT1_NUL.tmp
	echo "* Generated from CAT1_NUL.F using preprocessor $(FPP)" > CAT1_NUL.tmp
	echo "* DO NOT EDIT THIS FILE" >> CAT1_NUL.tmp
	echo "      INCLUDE 'PRM_PAR'" >> CAT1_NUL.tmp
	grep '^ .*[^ ]' $@ >> CAT1_NUL.tmp
	rm -f $@
	mv CAT1_NUL.tmp $@


## Standalone local test, requires that other components are installed.
## Note this doesn't test the ADAM libraries.

TESTS = cat_test
check_PROGRAMS = cat_test

cat_test_SOURCES = cat_test.f
cat_test_LDADD = libcat.la `cfitsio_link` `sla_link` `chr_link` `fio_link` \
                 `err_link` `psx_link` `ast_link`

##  Make sure output file is removed before running next test, also see if it
##  differs.
check-local:
	@if cmp -s testcat.TXT testcat.TXT.out; then :; else \
	   echo "*** TEST files DIFFER";\
	fi
	@rm -f testcat.TXT

EXTRA_DIST = $(PRIVATE_INCLUDES) CAT1_NUL.F

dist_starnews_DATA = cat.news
dist_pkgdata_DATA = LICENCE $(EXAMPLE_FILES)


# CAT has not (historically) generated unsigned int variants
.gen.f:
	$(FGENERIC) -t bcdiklrw -c CAT__SZVAL $<

.gn.f:
	$(FGENERIC) -t bdikrw $<

.gcn.f:
	$(FGENERIC) -t bcdikrw $<
