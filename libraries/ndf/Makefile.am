## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = ndf_link ndf_link_adam

lib_LTLIBRARIES = libndf.la libndf_adam.la

libndf_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(BASIC_F_ROUTINES) \
	$(GEN_F_ROUTINES) \
	$(STANDALONE_F_ROUTINES) \
	$(BASIC_C_ROUTINES) \
	$(STANDALONE_C_ROUTINES) \
	$(PRIVATE_INCLUDES)

libndf_la_LIBADD = `psx_link`

libndf_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(BASIC_F_ROUTINES) \
	$(GEN_F_ROUTINES) \
	$(ADAM_F_ROUTINES) \
	$(BASIC_C_ROUTINES) \
	$(ADAM_C_ROUTINES) \
	$(PRIVATE_INCLUDES)

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libndf_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libndf_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libndf_adam_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libndf_adam_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
endif

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = ndf_err.h NDF_ERR

# Include files that should be installed
PUBLIC_INCLUDES = ndf.h NDF_PAR NDF_FUNC_DEC NDF_FUNC_DEF NDF_FUNC

# Include files that are only needed for the build
PRIVATE_INCLUDES = NDF_ACB NDF_CONST NDF_DCB NDF_ELB NDF_FCB NDF_PCB \
	NDF_TCB NDF_TMP NDF_APB ndf1.h ndf_const.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  Fortran routines required for both the standalone and ADAM libraries.
#  Include the BLOCK data file

BASIC_F_ROUTINES = ndf1_init.f ndf1_a2p.f ndf1_absnt.f ndf1_accok.f    \
ndf1_acprp.f ndf1_acre.f ndf1_acrst.f ndf1_adcre.f                     \
ndf1_adext.f ndf1_adfrm.f ndf1_adini.f  ndf1_admap.f	               \
ndf1_adprp.f ndf1_adrst.f ndf1_adsbn.f ndf1_adstp.f ndf1_adtyp.f       \
ndf1_adump.f ndf1_affor.f ndf1_amap.f ndf1_amsg.f ndf1_anl.f	       \
ndf1_annpl.f ndf1_antmp.f ndf1_aprp.f ndf1_arst.f ndf1_asbnd.f	       \
ndf1_asetc.f ndf1_ast2h.f ndf1_atyp.f ndf1_aump.f ndf1_avcre.f	       \
ndf1_avext.f ndf1_avfrm.f ndf1_avmap.f ndf1_avprp.f	     	       \
ndf1_avrst.f ndf1_avsbn.f ndf1_avstp.f ndf1_avtyp.f ndf1_avump.f       \
ndf1_awcre.f ndf1_awext.f ndf1_awfrm.f ndf1_awini.f	               \
ndf1_awmap.f ndf1_awprp.f ndf1_awrst.f ndf1_awsbn.f	               \
ndf1_awstp.f ndf1_awtyp.f ndf1_awump.f ndf1_axlim.f ndf1_bad.f	       \
ndf1_bpp.f ndf1_cbfrm.f ndf1_ccpy.f ndf1_chacc.f	               \
ndf1_chftp.f ndf1_chhum.f ndf1_chmod.f ndf1_chxnm.f                    \
ndf1_clfor.f ndf1_cln.f ndf1_cmpbl.f ndf1_cmpfl.f ndf1_cpync.f	       \
ndf1_crfor.f ndf1_crnbn.f ndf1_cut.f ndf1_cvfor.f ndf1_cvtok.f	       \
ndf1_da.f ndf1_dac.f ndf1_dad.f ndf1_dan.f ndf1_danl.f                 \
ndf1_dav.f ndf1_daw.f ndf1_dbad.f ndf1_dc.f ndf1_dcre.f ndf1_dcrep.f   \
ndf1_dd.f ndf1_del.f ndf1_delob.f ndf1_dh.f ndf1_dimp.f ndf1_dlfor.f   \
ndf1_dmap.f ndf1_dmsg.f ndf1_dnfor.f ndf1_dq.f ndf1_dqanl.f	       \
ndf1_dump.f ndf1_dv.f ndf1_dvanl.f ndf1_dw.f ndf1_dx.f ndf1_ellip.f    \
ndf1_expid.f ndf1_exppl.f ndf1_evmsg.f ndf1_ffs.f ndf1_filex.f         \
ndf1_fmhdt.f ndf1_forxt.f ndf1_fsplt.f ndf1_gadex.f ndf1_gawex.f       \
ndf1_gtbb.f ndf1_gtenv.f ndf1_gthdt.f ndf1_gtxtn.f ndf1_h2ast.f	       \
ndf1_hcpy.f ndf1_hdcre.f ndf1_hderr.f ndf1_htlen.f ndf1_fr2px.f \
ndf1_hfwrt.f ndf1_hincr.f ndf1_hlerr.f ndf1_hnew.f ndf1_hprp.f	       \
ndf1_hrst.f ndf1_hscrt.f ndf1_htcmp.f ndf1_htop.f	       \
ndf1_hunmp.f ndf1_hwdef.f ndf1_hwrt.f ndf1_id2ac.f ndf1_imp.f	       \
ndf1_impid.f ndf1_imppl.f ndf1_indxp.f ndf1_infcb.f ndf1_inifr.f       \
ndf1_intcb.f ndf1_len.f ndf1_map.f ndf1_mbad.f ndf1_mbndp.f	       \
ndf1_mbndt.f ndf1_move.f ndf1_mtyp.f ndf1_mxtyp.f ndf1_ncut.f	       \
ndf1_nfind.f ndf1_nplac.f ndf1_nsplt.f ndf1_ntfor.f ndf1_nxtsl.f       \
ndf1_opfor.f ndf1_p2a.f ndf1_plcre.f ndf1_pldcb.f ndf1_plfor.f	       \
ndf1_prfor.f ndf1_prp.f ndf1_pscpx.f ndf1_psffl.f ndf1_psfmt.f	       \
ndf1_pshdt.f ndf1_psndb.f       \
ndf1_psnde.f ndf1_psndf.f ndf1_pstyp.f ndf1_pxlst.f ndf1_qbpp.f	       \
ndf1_qcre.f ndf1_qfrm.f ndf1_qimp.f ndf1_qityp.f ndf1_qma.f	       \
ndf1_qmap.f ndf1_qmlog.f ndf1_qrst.f ndf1_qsta.f	               \
ndf1_qump.f ndf1_rdast.f ndf1_rdtun.f ndf1_rdwcs.f ndf1_rjust.f	       \
ndf1_rls.f ndf1_rst.f ndf1_s2v.f ndf1_setc.f ndf1_simlr.f              \
ndf1_spfor.f ndf1_spldt.f ndf1_ssdup.f ndf1_tcnam.f ndf1_temp.f	       \
ndf1_trace.f ndf1_true.f ndf1_twrap.f ndf1_typ.f ndf1_ump.f ndf1_v2s.f \
ndf1_vaccn.f ndf1_van.f ndf1_vbad.f ndf1_vbnd.f	                       \
ndf1_vccn.f ndf1_vcpx.f ndf1_vcre.f ndf1_vdat.f ndf1_vfrm.f	       \
ndf1_vftp.f ndf1_vimp.f ndf1_vmap.f ndf1_vmmd.f ndf1_vmod.f	       \
ndf1_vrst.f ndf1_vsbd.f ndf1_vsftp.f ndf1_vsta.f ndf1_vstat.f	       \
ndf1_vtyp.f ndf1_vump.f ndf1_vwcs.f ndf1_wrast.f ndf1_wrwcs.f	       \
ndf1_wsbnd.f ndf1_wsta.f ndf1_wwrt.f ndf1_xcpy.f ndf1_xlst.f ndf1_xtfor.f \
ndf_acget.f ndf_aclen.f ndf_acmsg.f ndf_acput.f ndf_acre.f ndf_aform.f \
ndf_amap.f ndf_annul.f ndf_anorm.f ndf_arest.f ndf_asnrm.f ndf_astat.f \
ndf_astyp.f ndf_atype.f ndf_aunmp.f ndf_bad.f ndf_base.f ndf_bb.f      \
ndf_begin.f ndf_block.f ndf_bound.f ndf_cget.f ndf_chunk.f ndf_clen.f  \
ndf_clone.f ndf_cmplx.f ndf_cmsg.f ndf_copy.f ndf_cput.f ndf_delet.f   \
ndf_dim.f ndf_end.f ndf_find.f ndf_form.f ndf_ftype.f ndf_gtune.f      \
ndf_gtwcs.f ndf_happn.f ndf_hcre.f ndf_hdef.f ndf_hecho.f ndf_hend.f   \
ndf_hfind.f ndf_hinfo.f ndf_hnrec.f ndf_hout.f ndf_hpurg.f ndf_hput.f  \
ndf_hsmod.f ndf_imprt.f ndf_isacc.f ndf_isbas.f ndf_istmp.f ndf_loc.f  \
ndf_map.f ndf_mapql.f ndf_mapz.f ndf_mbad.f ndf_mbadn.f ndf_mbnd.f     \
ndf_mbndn.f ndf_msg.f ndf_mtype.f ndf_mtypn.f ndf_nbloc.f ndf_nchnk.f  \
ndf_new.f ndf_newp.f ndf_noacc.f ndf_open.f ndf_place.f ndf_ptwcs.f    \
ndf_qmf.f ndf_reset.f ndf_same.f ndf_sbad.f ndf_sbb.f ndf_sbnd.f       \
ndf_scopy.f ndf_sect.f ndf_shift.f ndf_size.f ndf_sqmf.f ndf_ssary.f   \
ndf_state.f ndf_stype.f ndf_temp.f ndf_trace.f ndf_tune.f ndf_type.f   \
ndf_unmap.f ndf_valid.f ndf_xdel.f ndf_xiary.f ndf_xloc.f  \
ndf_xname.f ndf_xnew.f ndf_xnumb.f ndf_xstat.f ndf1_cvcmd.f \
ndf1_farg.f ndf_sctyp.f ndf1_sctyp.f ndf1_vstyp.f ndf1_wcspm.f \
ndf1_wclim.f ndf1_wplim.f ndf1_mpanl.f ndf1_mpspt.f ndf_hgmod.f \
ndf_hsdat.f ndf1_chtim.f ndf1_hcopy.f ndf1_hsrt.f ndf_isin.f ndf_zscal.f \
ndf1_zpsca.f ndf1_vclst.f ndf_zdelt.f ndf1_cmpac.f ndf_gtdlt.f \
ndf_hcopy.f ndf1_stats.f

NDF_N_GEN = \
ndf1_adex.gn ndf1_adix.gn ndf1_avex.gn ndf1_awex.gn \
ndf1_awix.gn ndf1_bppx.gn ndf1_cvtx.gn ndf1_qmax.gn \
ndf1_s2vx.gn ndf1_v2sx.gn

NDF_DIKR_GEN = \
ndf_gtszx.gdikr ndf_ptszx.gdikr

NDF_CDIKRL_GEN = \
ndf_xgt0x.gcdikrl ndf_xpt0x.gcdikrl

GEN_F_ROUTINES = \
$(NDF_N_GEN:.gn=.f) $(NDF_DIKR_GEN:.gdikr=.f) $(NDF_CDIKRL_GEN:.gcdikrl=.f)

#  Additional Fortran routines required for the standalone library.

STANDALONE_F_ROUTINES = ndf1_hwenv.f

#  Additional Fortran routines required for the ADAM library.

ADAM_F_ROUTINES = ndf_assoc.f ndf_cinp.f ndf_creat.f ndf_crep.f	\
ndf_exist.f ndf_prop.f ndf1_getap_a.f ndf1_hwenv_a.f ndf_crepl.f \
ndf1_ptloc.f ndf_cancl.f

#  C routines required for both the standalone and ADAM libraries.

BASIC_C_ROUTINES = \
ndf1_docmd.c ndf1_expfn.c ndf1_filac.c ndf1_gtarg.c ndf1_gtfil.c \
ndf1_gtime.c ndf1_tilde.c ndf1.c ndf.c ndfinit.c ndf_hndlr.c \
ndf1_time.c ndf1_mjd2t.c ndf1_argvc.c ndf1_zscal.c

#  Additional C routines required for the standalone library.

STANDALONE_C_ROUTINES = ndf1_getap.c

#  Additional C routines required for the ADAM library.

ADAM_C_ROUTINES = ndf_adam.c


BUILT_SOURCES = $(include_MESSAGES)

# Testing

TESTS = ndf_ctest ndf_ftest ndf_cnulltest
check_PROGRAMS = ndf_ctest ndf_ftest ndf_cnulltest

ndf_ctest_SOURCES = ndf_ctest.c
ndf_ftest_SOURCES = ndf_ftest.f
ndf_cnulltest_SOURCES = ndf_cnulltest.c

# LDADD is the expansion of ref_link, with -lref replaced by libref.la
ndf_ctest_LDADD = libndf.la `$(srcdir)/ndf_link` `cnfrtl_link`  @FCLIBS@
ndf_ftest_LDADD = libndf.la `$(srcdir)/ndf_link`
ndf_cnulltest_LDADD = libndf.la `$(srcdir)/ndf_link` `cnfrtl_link`  @FCLIBS@

dist_starnews_DATA = ndf.news
dist_pkgdata_DATA = LICENCE

EXTRA_DIST = $(PRIVATE_INCLUDES)

.gn.f:
	$(FGENERIC)  -t n $<

.gdir.f:
	$(FGENERIC)  -t dir $<

.gcdirl.f:
	$(FGENERIC)  -t gcdirl $<

.gdikr.f:
	$(FGENERIC)  -t dikr $<

.gcdikrl.f:
	$(FGENERIC)  -t gcdikrl $<

