## Process this file with automake to produce Makefile.in

## subdirectory misc has a configure.ac and Makefile.am -- why is it not
## included in this list (or rather, the AC_CONFIG_SUBDIRS list)?
SUBDIRS = aif aspker atlib goodies io2ro_f kergen kersub mapgl obsrap_f \
    plt2d_f polrap_f rapgen rapi2d_f rapsub trace 

bin_SCRIPTS = ircamdr

bin_MONOLITHS = io2ro obsrap polrap plt2d rapi2d 

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Location to install ICL scripts and data files (per-package-bindir)
icldir = $(bindir)
icl_DATA = $(ICL_ROUTINES)

bindatadir = $(bindir)
bindata_DATA = $(DATA_FILES)

# Monoliths

io2ro_SOURCES = io2ro.f
# no variable io2ro_TASKS -- io2ro is an ATASK
io2ro_LDADD = io2ro_f/libio2ro.la `chr_link_adam` `hds_link_adam` \
	`fio_link_adam`

obsrap_SOURCES = obsrap.f
# no variable obsrap_TASKS -- obsrap is an ATASK
obsrap_LDADD = \
	obsrap_f/libobsrap.la \
	rapi2d_f/librapi2d.la \
	plt2d_f/libplt2d.la \
        aspker/libaspker.la \
	atlib/libatl.la \
	aif/libaif.la \
	rapsub/librapsub.la \
	rapgen/librapgen.la \
        kersub/libkersub.la \
	kergen/libkergen.la \
	`ndf_link_adam` `fio_link_adam` `mag_link_adam` `pda_link_adam`

polrap_SOURCES = polrap.f
# no variable polrap_TASKS -- polrap is an ATASK
polrap_LDADD = \
polrap_f/libpolrap.la \
obsrap_f/libobsrap.la \
rapi2d_f/librapi2d.la \
plt2d_f/libplt2d.la \
aspker/libaspker.la \
atlib/libatl.la\
aif/libaif.la \
trace/libtrace.la \
rapsub/librapsub.la \
rapgen/librapgen.la \
kersub/libkersub.la \
kergen/libkergen.la \
`ndf_link_adam` `fio_link_adam` `pda_link_adam`

AM_FCFLAGS = -Iplt2d_f

plt2d_SOURCES = plt2d.f
# no variable plt2d_TASKS -- plt2d is an ATASK
plt2d_LDADD = \
	plt2d_f/libplt2d.la \
	goodies/libgoodies.la \
	atlib/libatl.la \
	`ndf_link_adam` `sgs_link_adam` \
	`fio_link_adam` `gks_link_adam`

rapi2d_SOURCES = rapi2d.f
# no variable rapi2d_TASKS -- rapi2d is an ATASK
rapi2d_LDADD = \
	rapi2d_f/librapi2d.la \
	aspker/libaspker.la \
	atlib/libatl.la \
	aif/libaif.la \
	trace/libtrace.la \
	rapsub/librapsub.la \
	rapgen/librapgen.la \
	kersub/libkersub.la \
	kergen/libkergen.la \
	`ndf_link_adam` `fio_link_adam` `pda_link_adam`

# ICL scripts

ICL_ROUTINES = icl/ablock.icl icl/accoff.icl icl/again.icl	       \
icl/amcorrlot.icl icl/anncol.icl icl/applymasklot.icl		       \
icl/array_tests.icl icl/border.icl icl/box.icl icl/cablock.icl	       \
icl/calexp.icl icl/calmag.icl icl/calzer.icl icl/cbox.icl	       \
icl/ccircle.icl icl/ccross.icl icl/ccut.icl icl/cellipse.icl	       \
icl/cent1.icl icl/cent2.icl icl/chdisp.icl icl/chred.icl	       \
icl/circle.icl icl/clear.icl icl/clearit.icl icl/cline.icl	       \
icl/cnsigma.icl icl/coff.icl icl/colinv.icl icl/coll.icl	       \
icl/coltab.icl icl/cont_title.icl icl/contoff.icl icl/contour.icl      \
icl/cplot.icl icl/cranplot.icl icl/cross.icl icl/crosscut.icl	       \
icl/crosscut_peak.icl icl/cstats.icl icl/curhot.icl icl/cursor.icl     \
icl/cut.icl icl/cut2ff.icl icl/cut_title.icl icl/cvargrey.icl	       \
icl/cxwin.icl icl/daocen.icl icl/daofind.icl icl/daogid.icl	       \
icl/daogid2.icl icl/darklot.icl icl/define_atasks.icl icl/defprocs.icl \
icl/deglot.icl icl/disp.icl icl/dispick.icl icl/ellipse.icl	       \
icl/exit.icl icl/fclose_a.icl icl/fclose_b.icl icl/fclose_c.icl	       \
icl/fclose_d.icl icl/fclose_e.icl icl/flat2.icl icl/flatlot.icl	       \
icl/focrun.icl icl/focrun2.icl icl/formname.icl icl/formname2.icl      \
icl/fpstriplot.icl icl/get_imagename.icl icl/get_imagename_old.icl     \
icl/getcm.icl icl/getoff.icl icl/getrast.icl icl/glitchmark.icl	       \
icl/glkill.icl icl/global_vars.icl icl/god_ircamdr_starlink.icl	       \
icl/god_ircamdr_ukirt.icl icl/grid.icl icl/hardannot.icl	       \
icl/hardcom.icl icl/hardcopy.icl icl/hardcopy_cv.icl		       \
icl/hardcopy_il.icl icl/hardcopy_ilv.icl icl/hardcopy_image.icl	       \
icl/hardcopy_iv.icl icl/hardcopy_line.icl icl/hardlot.icl	       \
icl/hardprint.icl icl/hardprint_unix.icl icl/hardsym.icl icl/io2ro.icl \
icl/jitreg.icl icl/lagain.icl icl/limag.icl icl/line.icl	       \
icl/line_width.icl icl/linecol.icl icl/logo.icl icl/lplt.icl	       \
icl/mcursor.icl icl/medlot.icl icl/mofflot.icl icl/moonraw.icl	       \
icl/morensigma.icl icl/moreplot.icl icl/moreranplot.icl		       \
icl/morevargrey.icl icl/mos2.icl icl/nomag.icl icl/nsigma.icl	       \
icl/oadd.icl icl/objproc.icl icl/objred.icl icl/ocadd.icl	       \
icl/ocdiv.icl icl/ocmult.icl icl/ocstats.icl icl/ocsub.icl	       \
icl/odist.icl icl/odiv.icl icl/ohisto.icl icl/olook.icl icl/omult.icl  \
icl/ostats.icl icl/osub.icl icl/pclose.icl icl/pencol.icl	       \
icl/penint.icl icl/pho.icl icl/pho2.icl icl/pho3.icl icl/picklot.icl   \
icl/plot.icl icl/plotglitch.icl icl/plotlot.icl icl/pol2.icl	       \
icl/polflat.icl icl/polpick2.icl icl/polreg.icl icl/polseplot.icl      \
icl/polshift2.icl icl/polsky2.icl icl/popen.icl icl/popen_new.icl      \
icl/popen_unix.icl icl/psf_test.icl icl/ranplot.icl		       \
icl/reload_obsrap.icl icl/remos.icl icl/rflot.icl icl/rocent.icl       \
icl/rodarklot.icl icl/roindex.icl icl/romed.icl icl/ropar.icl	       \
icl/rotlot.icl icl/rout.icl icl/satlot.icl icl/satpho.icl	       \
icl/satraw.icl icl/satsub.icl icl/scaledark.icl icl/scalelot.icl       \
icl/see.icl icl/setarea.icl icl/setcen.icl icl/setcomori.icl	       \
icl/setcont.icl icl/setcontic.icl icl/setcur.icl icl/setcurmark.icl    \
icl/setcut.icl icl/setcutaxrat.icl icl/setfile.icl		       \
icl/setfile_unix.icl icl/setfont.icl icl/sethard.icl icl/setmag.icl    \
icl/setmax.icl icl/setmin.icl icl/setmm.icl icl/setnum.icl	       \
icl/setnumori.icl icl/setnumsca.icl icl/setpolcol.icl icl/setpre.icl   \
icl/setps.icl icl/setquad.icl icl/setradec.icl icl/setstd.icl	       \
icl/setvar.icl icl/setvargrey.icl icl/setvec.icl icl/shift2.icl	       \
icl/shift3.icl icl/short_display.icl icl/skyext.icl icl/skysub.icl     \
icl/skysub4.icl icl/splitlot.icl icl/stdpho.icl icl/stdproc.icl	       \
icl/stdred.icl icl/stlot.icl icl/stred.icl icl/strehl.icl	       \
icl/striplot.icl icl/surround.icl icl/testval.icl icl/tickio.icl       \
icl/ticklen.icl icl/tomag.icl icl/tslot.icl icl/utils.icl	       \
icl/utils_old.icl icl/utils_unix.icl icl/valstr.icl icl/valstr_old.icl \
icl/vans.icl icl/vargrey.icl icl/vcen.icl icl/vcor.icl icl/vdel.icl    \
icl/vec.icl icl/vec_title.icl icl/vsub.icl icl/wraplot.icl	       \
icl/wrccom.icl icl/wrcom.icl icl/writelut.icl icl/x2mag.icl	       \
icl/x5mag.icl icl/zaplot.icl

#  List of ancilliary data files.

DATA_FILES = $(D_SUBX) $(D_SUBY) $(D_SUBZ) $(D_SUBW)

D_SUBW = data/ircam3_coll.dat

D_SUBX = data/bpm_fpa42_128x128.sdf data/bpm_fpa42_256x256.sdf	       \
data/bpm_fpa42_64x64.sdf data/col1.sdf data/col10.sdf data/col11.sdf   \
data/col12.sdf data/col13.sdf data/col14.sdf data/col15.sdf	       \
data/col16.sdf data/col17.sdf data/col18.sdf data/col19.sdf	       \
data/col2.sdf data/col20.sdf data/col21.sdf data/col22.sdf	       \
data/col23.sdf data/col24.sdf data/col25.sdf data/col26.sdf	       \
data/col27.sdf data/col28.sdf data/col29.sdf data/col3.sdf	       \
data/col30.sdf data/col31.sdf data/col32.sdf data/col33.sdf	       \
data/col34.sdf data/col35.sdf data/col36.sdf data/col37.sdf	       \
data/col38.sdf data/col39.sdf data/col4.sdf data/col40.sdf	       \
data/col41.sdf data/col42.sdf data/col43.sdf data/col44.sdf	       \
data/col45.sdf data/col46.sdf data/col47.sdf data/col48.sdf	       \
data/col49.sdf data/col5.sdf data/col6.sdf data/col7.sdf data/col8.sdf \
data/col9.sdf data/colb.sdf data/colc.sdf data/colg.sdf data/colo.sdf  \
data/colp.sdf data/colr.sdf data/cols.sdf data/coly.sdf data/grey.sdf  \
data/heat.sdf data/logo.sdf data/pony.sdf

# fp.dat was in the original ircamdr distribution but now lost

D_SUBY = data/fs.dat data/ukirt.dat data/spiral.off

D_SUBZ = data/bad_objects.list data/extinct.list			 \
data/lincoeff_fpa118_postjul88.list				 \
data/lincoeff_fpa118_postjul88_ndr.list				 \
data/lincoeff_fpa118_prejul88.list				 \
data/lincoeff_fpa118_prejul88_ndr.list data/lincoeff_fpa175.list \
data/lincoeff_fpa175_ndr.list
