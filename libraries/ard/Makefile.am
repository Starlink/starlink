## Process this file with automake to produce Makefile.in

cincludedir = $(includedir)/star

lib_LTLIBRARIES = libard.la libard_adam.la
dist_bin_SCRIPTS = ard_link ard_link_adam

libard_la_SOURCES = \
        $(PUBLIC_INCLUDES) $(PRIVATE_INCLUDES) $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) $(STANDALONE_F_ROUTINES) $(BLOCKDATA)

libard_adam_la_SOURCES = \
        $(include_MESSAGES) $(PRIVATE_INCLUDES) $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) $(ADAM_F_ROUTINES) $(BLOCKDATA)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

include_MESSAGES = ARD_ERR ard_err.h
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

BUILT_SOURCES = ARD_ERR ard_err.h

#  Define test programs.
TESTS = test1 test2
check_PROGRAMS = test1 test2
test1_SOURCES = ard_test.f
test1_LDADD = libard.la `ast_link` `sla_link` `grp_link` `chr_link` \
	`err_link`  `psx_link`

test2_SOURCES = ard_ctest.c
test2_LDADD = libard.la `ast_link` `sla_link` `grp_link` `chr_link` \
	`err_link`  `psx_link` @FCLIBS@

PUBLIC_C_INCLUDES = ard.h

PRIVATE_INCLUDES = ARD_COM ARD_CONST


BASIC_F_ROUTINES = \
  ard1_adanl.f ard1_altrp.f ard1_and.f ard1_andbx.f ard1_apwcs.f \
  ard1_args2.f ard1_args.f ard1_asmrg.f ard1_box.f ard1_boxar.f \
  ard1_bxand.f ard1_bxbox.f ard1_bxcir.f ard1_bxcpy.f ard1_bxeqv.f \
  ard1_bxfnd.f ard1_bxor.f ard1_bxset.f ard1_bxxor.f ard1_check.f \
  ard1_cir.f ard1_cirar.f ard1_col.f ard1_colar.f ard1_copyd.f \
  ard1_cowcs.f ard1_ctov.f ard1_draw2.f ard1_ell.f ard1_ellar.f \
  ard1_eqv.f ard1_field.f ard1_fra.f ard1_fraar.f ard1_gattr.f \
  ard1_gcap.f ard1_gline.f ard1_gtarg.f ard1_gtov.f ard1_intrb.f \
  ard1_intra.f ard1_intr.f ard1_invrs.f ard1_kdraw.f ard1_keyw.f \
  ard1_lin.f ard1_lin2.f ard1_linar.f ard1_linfl.f ard1_linmp.f \
  ard1_lkr.f ard1_lnr.f ard1_lsm.f ard1_ltran.f ard1_match.f ard1_dstax.f \
  ard1_merge.f ard1_nlnr.f ard1_not.f ard1_ofwcs.f ard1_or.f ard1_scale.f \
  ard1_orbx.f ard1_poi.f ard1_poi2.f ard1_poiar.f ard1_pol.f \
  ard1_polar.f ard1_putd.f ard1_puti.f ard1_putr.f ard1_rdcof.f \
  ard1_rdwcs.f ard1_rec.f ard1_recar.f ard1_rot.f ard1_rotar.f \
  ard1_row.f ard1_rowar.f ard1_rwcl.f ard1_scwcs.f ard1_sfbnd.f \
  ard1_srcta.f ard1_stat.f ard1_storr.f ard1_storp.f ard1_stord.f \
  ard1_stori.f ard1_stwcs.f ard1_trans.f ard1_trcop.f ard1_trcon.f \
  ard1_truni.f ard1_twwcs.f ard1_ubbox.f ard1_uinterp.f ard1_who.f \
  ard1_xor.f ard1_snkta.f ard_grpex.f ard_wcs.f ard_work.f ard_ptwcs.f \
  ard1_crfnd.f ard1_get.f ard1_assim.f ard_plot.f ard_gtwcs.f \
  ard1_fplot.f ard1_ceil.f ard1_dmwcs.f \
  ard1_cntdr.f ard1_eval.F ard_work8.f

BASIC_C_ROUTINES = ard.c

STANDALONE_F_ROUTINES =

ADAM_F_ROUTINES = ard_group.f

BLOCKDATA = ard1_init.f

dist_starnews_DATA = ard.news
dist_pkgdata_DATA = LICENCE
