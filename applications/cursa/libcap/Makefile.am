## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libcap.la

libcap_la_SOURCES = $(F_ROUTINES) $(ATASK_ROUTINES)


F_ROUTINES = \
 cap_adpar.f   cap_crtar.f   cap_ghssh.f   cap_gtstr.f   cap_prgrt.f  \
 cap_airms.f   cap_crtcp.f   cap_gids.f    cap_gwtfl.f   cap_pritm.f  \
 cap_angdc.f   cap_cutob.f   cap_ginit.f   cap_hsdet.f   cap_prlin.f  \
 cap_arcdc.f   cap_dceqp.f   cap_gisan.f   cap_info.f    cap_prng.f  \
 cap_caxlb.f   cap_ellips.f  cap_glcol.f   cap_jtwrt.f   cap_pscat.f  \
 cap_cctmm.f   cap_envsm.f   cap_glfmt.f   cap_lstas.f   cap_qsrtd.f  \
 cap_cctmo.f   cap_expcl.f   cap_glist.f   cap_lstcl.f   cap_r2sgf.f  \
 cap_cflph.f   cap_fitph.f   cap_gnhst.f   cap_lstnm.f   cap_range.f  \
 cap_cfpar.f   cap_frear.f   cap_gnsgt.f   cap_lstpr.f   cap_rascl.f  \
 cap_circ.f    cap_ganct.f   cap_gpcls.f   cap_lstsm.f   cap_rdclr.f  \
 cap_circl.f   cap_gcdet.f   cap_gprev.f   cap_lsttx.f   cap_rdply.f  \
 cap_clcnt.f   cap_gcdsp.f   cap_grows.f   cap_modet.f   cap_rdsct.f  \
 cap_clcrd.f   cap_gcexp.f   cap_gscat.f   cap_nsamp.f   cap_rdtph.f  \
 cap_clint.f   cap_gcids.f   cap_gsccl.f   cap_opgtt.f   cap_rect.f  \
 cap_clval.f   cap_gcini.f   cap_gscmp.f   cap_out.f     cap_resph.f  \
 cap_cmpmo.f   cap_gclse.f   cap_gscnf.f   cap_outf.f    cap_rflph.f  \
 cap_cpcat.f   cap_gcols.f   cap_gscol.f   cap_pacol.f   cap_rjlst.f  \
 cap_cpccl.f   cap_gcpax.f   cap_gscop.f   cap_pair.f    cap_rpfmt.f  \
 cap_cpcol.f   cap_gcpid.f   cap_gscpl.f   cap_pairt.f   cap_rpscl.f  \
 cap_cpcph.f   cap_gcprs.f   cap_gscrn.f   cap_pgett.f   cap_saxes.f  \
 cap_cpcrd.f   cap_gcrng.f   cap_gscsh.f   cap_phist.f   cap_sclbr.f  \
 cap_cpfld.f   cap_gcsel.f   cap_gsctb.f   cap_pltdt.f   cap_sepeq.f  \
 cap_cpnul.f   cap_gdcol.f   cap_gsfil.f   cap_pltga.f   cap_spoly.f  \
 cap_cppar.f   cap_gdcpl.f   cap_gspar.f   cap_pltgr.f   cap_staet.f  \
 cap_cpprs.f   cap_gdpar.f   cap_gsrng.f   cap_pltgs.f   cap_stsym.f  \
 cap_cpscl.f   cap_gfcol.f   cap_gssel.f   cap_pltlg.f   cap_tblph.f  \
 cap_cpsco.f   cap_gfile.f   cap_gstar.f   cap_pltst.f   cap_tpcol.f  \
 cap_cptab.f   cap_gfout.f   cap_gstat.f   cap_plysl.f   cap_tpsym.f  \
 cap_cptgc.f   cap_gfpar.f   cap_gstax.f   cap_popg1.f   cap_txtph.f  \
 cap_cptrg.f   cap_ghelp.f   cap_gstrd.f   cap_popg2.f   cap_warn.f  \
 cap_cptxt.f   cap_ghsop.f   cap_gstrw.f   cap_popg3.f  \
 cap_crnsp.f   cap_ghspl.f   cap_gstxt.f   cap_prdet.f  \
 cap_crtab.f   cap_ghsrn.f   cap_gtgdc.f   cap_prerr.f

ATASK_ROUTINES = \
 catchart.f  catchartrn.f  catcoord.f  catcopy.f  catgrid.f  \
 catgscin.f  catheader.f   catpair.f  catphotomfit.f  catphotomlst.f  \
 catphotomtrn.f  catselect.f  catsort.f

PRIVATE_INCLUDES = SGZ_PAR SGZ_CMN SPLOT_CMN CIO_PAR CIO_CMN CHART_PAR \
                   CHART_CMN CAP_PAR

EXTRA_DIST = $(PRIVATE_INCLUDES)

