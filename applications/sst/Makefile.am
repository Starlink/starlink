## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = $(DYNSCRIPTS)

bin_MONOLITHS = sst_mon
sst_mon_SOURCES = \
	sst_mon.f \
	$(TASK_ROUTINES) \
	$(SUBSRC) \
	$(PRIVATE_INCLUDES)

sst_mon_TASKS = forstats procvt prohlp prolat propak prohtml
sst_mon_LDADD = $(LDADD) `fio_link_adam` `one_link_adam`

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
starhelp_DATA = sst.shl

TASK_ROUTINES = $(sst_mon_TASKS:=.f)
IFL_FILES = $(sst_mon_TASKS:=.ifl)
DYNSCRIPTS = sst.csh sst.sh sst.icl

#  Generate ifl and startup scripts from the ifd file and then do any 
#  necessary post processing of the generated files to overcome 
#  deficiencies in ifd2star
$(IFL_FILES) $(DYNSCRIPTS): sst.ifd
	$(IFD2STAR) sst

.hlp.shl:
	@HLIB@ $<

# Build a help system from the newly built application
sst.hlp: $(TASK_ROUTINES) prolat.ifl sst_mon sst_master.hlp
	-rm -f sst.hlp all.f sst_tmp.hlp prohlp
	cat $(TASK_ROUTINES) > all.f
	$(LN_S) sst_mon prohlp
	./prohlp in=all.f atask=yes out=sst_tmp.hlp single=no
	cat sst_master.hlp sst_tmp.hlp > sst.hlp
	-rm -f all.hlp all.f prohlp sst_tmp.hlp

SUBSRC = sst_clean.f sst_fwild.f sst_latex.f sst_puts.f  sst_trcvt.f \
         sst_cntac.f sst_get.f sst_latp.f sst_rdad1.f sst_trhlp.f \
         sst_for.f sst_gtend.f sst_lats.f sst_rdad2.f sst_trlat.f \
         sst_forp.f sst_gtpun.f sst_nsect.f sst_rdpro.f sst_trpak.f \
         sst_fors.f sst_hic.f sst_okchr.f sst_rmchr.f sst_zapap.f \
         sst_fparx.f sst_hlpap.f sst_pargl.f sst_skchr.f sst_zappl.f \
         sst_analp.f sst_fsect.f sst_hlpky.f sst_put.f sst_stlat.f \
         sst_capwd.f sst_fskey.f sst_lat.f sst_putp.f sst_trchr.f \
         sst_html.f sst_htmlp.f sst_htmls.f sst_htmlx.f sst_trhtm.f \
         sst_sthtm.f sst_nwext.f sst_sysnm.f

PRIVATE_INCLUDES = SST_PAR SST_SCB


# We (oddly) want these data files to go into the same directory as the 
# binaries.  Since we have STAR_DEFAULTS(per-package-dirs) in configure.ac, 
# however, we are allowed to use bin_DATA (don't ask).
dist_bindata_DATA = sst_preamble.tex forstats.dat html.sty
bindatadir = $bindir


EXTRA_DIST = sst.ifd.in $(bin_DATA) sst_master.hlp

CLEANFILES = forstats.ifc procvt.ifc prohlp.ifc prohtml.ifc prolat.ifc \
             propak.ifc sst_mon start version.dat sst_mon.ifc sst_mon.ifl \
             forstats.ifl procvt.ifl prohlp.ifl prohtml.ifl prolat.ifl
