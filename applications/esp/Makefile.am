## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = esp_link_adam esp.csh esp.sh

bin_MONOLITHS = esp_mon
esp_mon_TASKS = corr ellfou ellpro esphelp fastmed \
    gaufit graphs histpeak hsub loback mask \
    mixup sector selfc selfcw skew topped

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

esp_mon_SOURCES = \
    esp_mon.f \
    $(esp_mon_TASKS:=.f) \
    $(TASK_SUBS) \
    $(COMMON_SUBS) \
    $(PSEUDOCOMMON_SUBS) \
    $(MISC_SUBS) \
    $(PAR_FILES)

esp_mon_LDADD = `$(srcdir)/esp_link_adam`

# Routines which are called by only one task or subroutine.  Although
# splitting these into separate files can be justified on grounds of
# tidiness, the real reason for keeping these separated from their
# corresponding task.f file is that at least one compiler (g77) gets
# upset when it sees a conflict between a call-by-reference argument in a
# subroutine declaration and a call-by value (that is, a use of %val())
# in a call of that subroutine.  This happens when we allocate arrays
# dynamically, and is a large part of the point of %val.  What a
# depressingly stupid compiler.
TASK_SUBS = \
    cor1_corr.f \
    elf1_hilow.f elf1_level.f elf1_find.f \
    fas1_filt.f \
    gau1_build.f gau1_build2.f \
    gau2_calc.f gau2_getrd.f gau2_outim.f gau2_rep.f \
    gau2_donsg.f gau2_inita.f gau2_prep.f gau2_uinit.f \
    gau2_errs.f gau2_initv.f gau2_pro.f gau2_xerrs.f \
    his1_selar.f \
    lob1_fill.f \
    mix1_mixer.f \
    sel1_selfc.f sel1_selfcw.f \
    ske1_global.f ske1_local.f \
    top1_remov.f \
    esp1_ard_drive2.f esp1_median.f

# Routines common to more than one task or helper subroutine.  Several
# of these subroutines (esp1_asffr, esp1_dsfrm, esp1_gtpos, kpg1_pgesc
# at least) are from kaplibs (back when it was still kappa/kapgen),
# but with modifications which prevent them being easily replaced by a
# simple call to that library.
COMMON_SUBS = \
    esp1_aif_asfio.f esp1_ard_driver.f \
    esp1_asffr.f esp1_crpt.f esp1_dsfrm.f esp1_gtpos.f esp1_gtpsz.f \
    esp1_inpos.f esp1_pr2s.f esp1_s2pr.f esp1_xyfmt.f esp1_farr.f \
    esp1_cato.f kpg1_pgesc.f

# The following modules contain multiple subroutines, with matching
# names and with more or less similar code, each of which is called by
# only one of the task subroutines.  Each of these sets of subroutines
# clearly share a common lineage, and are therefore ripe for
# refactoring, some much more so than others.  In the absence of a
# battery of unit tests, however, this is not a trivial exercise.
PSEUDOCOMMON_SUBS = \
    esp1_agic2.f esp1_agico.f esp1_angles.f esp1_autol.f esp1_cancl.f \
    esp1_chord.f esp1_cmode.f esp1_conv.f esp1_curso.f esp1_filer.f \
    esp1_fmode.f esp1_four.f esp1_gaujo.f esp1_gener.f esp1_graph.f \
    esp1_grbit.f esp1_guess.f esp1_hiloa.f esp1_inter0.f esp1_kmode.f \
    esp1_linre.f esp1_medmo.f esp1_messg.f esp1_momde.f esp1_para.f \
    esp1_prpcur.f esp1_rand.f esp1_rotat.f esp1_solve.f esp1_stats.f \
    esp1_textd.f esp1_texto.f esp1_trans.f esp1_uncon.f

# Other subroutines, which don't fit into any of the above categories.
MISC_SUBS = \
    dnsg.f esp1_vers.f

PAR_FILES = \
    COR_PAR ELF_PAR ELP_PAR FAS_PAR GAU_PAR GRA_PAR HIS_PAR HSB_PAR \
    LOB_PAR SEC_PAR SEL_PAR SKE_PAR TOP_PAR


# Help files
esp.shl: esp.hlp
	$(HLIB) esp.hlp
HELP_SOURCES = esp.hlp

starhelp_DATA = esp.shl

EXTRA_DIST = $(HELP_SOURCES)

dist_starnews_DATA = esp.news

IFC_FILES = corr.ifc ellfou.ifc ellpro.ifc esphelp.ifc fastmed.ifc gaufit.ifc \
graphs.ifc histpeak.ifc hsub.ifc loback.ifc mask.ifc mixup.ifc \
sector.ifc selfc.ifc selfcw.ifc skew.ifc topped.ifc esp_mon.ifc

MISC_BUILT_FILES = version.dat dtask_applic.lo esp_mon.ifl esp_mon esp.shl

