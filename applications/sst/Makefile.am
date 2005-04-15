## Process this file with automake to produce Makefile.in

bin_SCRIPTS = start

bin_MONOLITHS = sst_mon
sst_mon_SOURCES = \
	sst_mon.f \
	$(sst_mon_TASKS:=.f) \
	$(SUBSRC) \
	$(PRIVATE_INCLUDES)

sst_mon_TASKS = forstats procvt prohlp prolat propak prohtml
sst_mon_LDADD = $(LDADD) `fio_link_adam` `one_link_adam`

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@


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
bin_DATA = sun.tex sst.tex layout.tex forstats.dat html.sty

# The `start' script needs to have installation locations edited into it
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@VERSION\@,$(VERSION),g'
start: start.in
	rm -f start.tmp start
	$(edit) \
	    -e 's,@edited_input\@,start: produced from start.in by Makefile.am,' \
	    $(srcdir)/start.in >start.tmp
	mv start.tmp start

EXTRA_DIST = start.in $(bin_DATA)
