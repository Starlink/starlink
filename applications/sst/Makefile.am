## Process this file with automake to produce Makefile.in

bin_SCRIPTS = start

bin_MONOLITHS = sst_mon
sst_mon_SOURCES = \
	sst_mon.f \
	$(sst_mon_TASKS:=.f) \
	$(SUBSRC) \
	$(SUBCSRC) \
	$(PRIVATE_INCLUDES)
sst_mon_TASKS = forstats procvt prohlp prolat propak prohtml
sst_mon_LDFLAGS = $(LDFLAGS) `fio_link`


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

SUBCSRC = find_file.c

PRIVATE_INCLUDES = SST_PAR SST_SCB

# We (oddly) want these data files to go into the same directory as the 
# binaries, but automake doesn't allow `bin_DATA'.  However automake does
# supply an escape hatch, and allows `anything_DATA' as long as a
# another variable `anythingdir' is defined (presumably on the grounds that
# defining two matching variables lets it guess you haven't just made a
# mistake).  So define sstsupportdir to be $(bindir) -- the standard install
# location of the binaries -- and define sstsupport_DATA to tell automake
# the list of files to install there.
sstsupportdir = $(bindir)
sstsupport_DATA = sun.tex sst.tex layout.tex forstats.dat html.sty

