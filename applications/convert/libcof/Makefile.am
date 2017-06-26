## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libcof.la

libcof_la_SOURCES = \
 $(F_ROUTINES) $(F_GEN_FILES) $(INCLUDE_FILES)

F_ROUTINES = \
 cof_2dfex.f cof_2dfif.f cof_2dfim.f cof_addp.f cof_asc2ht.f cof_astwn.f \
 cof_atypc.f cof_bn2ht.f cof_bp2ht.f cof_camaa.f cof_camax.f cof_chisr.f \
 cof_chkp.f cof_chkxt.f cof_cophd.f cof_dateh.f cof_doscl.f cof_dstyp.f \
 cof_exdim.f cof_extab.f cof_extnf.f cof_extxt.f cof_f2ndf.f cof_fd2ht.f \
 cof_fhist.f cof_frmt.f cof_ft2ne.f cof_ftopr.f cof_gxtnm.f \
 cof_h2bin.f cof_halog.f cof_hdlog.f cof_ht2bn.f \
 cof_ines.f cof_iswcs.f cof_iuemh.f cof_iuemx.f cof_iueq.f cof_iuesi.f \
 cof_lws1.f cof_lws2.f cof_lwsan.f cof_mandh.f cof_mrghd.f cof_ndf2f.f \
 cof_ndfax.f cof_ndfcc.f cof_ndxt.f cof_nhead.f cof_rhead.f cof_savhd.f \
 cof_sbnd.f cof_spec.f cof_stypc.f cof_stype.f cof_swsaa.f cof_t2hds.f \
 cof_thier.f cof_wfexf.f cof_wfext.f cof_wgbfe.f cof_whead.f \
 cof_wndfh.f cof_wrtab.f cof_ftwcs.f cof_fpwcs.f cof_fndfs.f \
 cof_wcsax.f cof_wcsdf.f cof_wcsex.f cof_wcsim.f cof_wcsut.f cof_mghdr.f \
 cof_getfc.f cof_getyp.f cof_stscl.f cof_smurf.f cof_nex2f.f \
 cof_fi2ne.f cof_wstr.f cof_smfim.f cof_genam.f cof_wenam.f cof_tbsrc.f \
 cof_mhisr.f cof_issmf.f

GEN_ROUTINES = $(DIRW_GEN_ROUTINES) $(CDILR_GEN_ROUTINES)

F_GEN_FILES = $(DIRW_GEN_ROUTINES:.gdirw=.f) $(CDILR_GEN_ROUTINES:.gcdilr=.f)

INCLUDE_FILES = F2NDF1_CMN F2NDF2_CMN F2NDF3_CMN

##  Double, integer, real and word.
DIRW_GEN_ROUTINES = cof_escox.gdirw cof_mxmnx.gdirw cof_scofx.gdirw

##  Character, double, integer, logical and real.
CDILR_GEN_ROUTINES = cof_gkeyx.gcdilr

# Rules for generating Fortran from generic code.
FGENERIC=@FGENERIC@

.gdirw.f:
	$(LN_S) $< $(<:.gdirw=.gen)
	$(FGENERIC) -t dirw $(<:.gdirw=.gen)
	$(RM) $(<:.gdirw=.gen)

.gcdilr.f:
	$(LN_S) $< $(<:.gcdilr=.gen)
	$(FGENERIC) -t cdilr $(<:.gcdilr=.gen)
	$(RM) $(<:.gcdilr=.gen)


##  The Fortran intermediary files should be cleaned away sometime.
##  Since they are distributed make this maintainer-clean.
MAINTAINERCLEANFILES = $(F_GEN_FILES)
