## Process this file with automake to produce Makefile.in

bin_PROGRAMS = s_refresh
bin_SCRIPTS = $(DYNSCRIPTS) $(PUBLICSCRIPTS)
bin_MONOLITHS = starman1_mon starman2_mon starman3_mon starman4_mon
staretc_DATA = $(DATAFILES)

s_refresh_SOURCES = s_refresh.c
s_refresh_LDADD = `starx_link`

starman1_mon_SOURCES = starman1_mon.f
starman2_mon_SOURCES = starman2_mon.f
starman3_mon_SOURCES = starman3_mon.f
starman4_mon_SOURCES = starman4_mon.f

starman1_mon_LDADD = libstarman.la \
	`ndf_link_adam` `pgp_link_adam` \
	`sla_link_adam` `hlp_link_adam` `starx_link`
starman2_mon_LDADD = libstarman.la \
	`ndf_link_adam` `pgp_link_adam` \
	`sla_link_adam` `hlp_link_adam` `starx_link` `one_link_adam`
starman3_mon_LDADD = libstarman.la \
	`ndf_link_adam` `pgp_link_adam` \
	`sla_link_adam` `hlp_link_adam` `starx_link`
starman4_mon_LDADD = libstarman.la \
	`ndf_link_adam` `pgp_link_adam` \
	`sla_link_adam` `hlp_link_adam` `starx_link`

noinst_LTLIBRARIES = libstarman.la
libstarman_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES) $(BLOCKDATA)

starman1_mon_TASKS = $(TASK1)
starman2_mon_TASKS = $(TASK2)
starman3_mon_TASKS = $(TASK3)
starman4_mon_TASKS = $(TASK4)

TASK1 = addstars automag average chi diagram dustring imcalc imcube \
imcut imdes imfits_dr imflash

TASK2 = imjoin imkey import imrotate imsmooth imstat imtype imweed \
interact measure profile simplemag sprinkle starfind starmangripe \
starmanhelp

TASK3 = tbcalc tbchart tbcomps tbcut tbdes tbjoin tbkey tblist tbload \
tbmatch tbnative tbnmatch tbplot tbpmatch tbrenum

TASK4 = tbsheet tbsort tbstat tbtran_auto tbtran_do tbtran_load \
tbtran_make tbvalue tbweed unccd

IFL_FILES = $(TASKS:=.ifl) starman1_mon.ifl \
	starman2_mon.ifl starman3_mon.ifl starman4_mon.ifl

PUBLICSCRIPTS = starmandemo starmanhypergripe starmanhyperhelp

DYNSCRIPTS = starman.csh starman.sh starman.icl

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

F_ROUTINES = st_lib_sou.f st_lib_1_sou.f st_lib_2_sou.f st_lib_d_sou.f \
         addstars_sou.f automag_sou.f average_sou.f \
         chi_sou.f diagram_sou.f dustring_sou.f \
         image_sou.f interact_sou.f measure_sou.f  \
         profile_sou.f simplemag_sou.f \
         sprinkle_sou.f starfind_sou.f table_sou.f \
         unccd_sou.f stargripe_sou.f starhelp_sou.f

BLOCKDATA = ds_lut_bl.f

C_ROUTINES = dsx_genc.c dsx_panelc.c dsx_lutc.c dsx_utilc.c \
	vt_genc.c

PRIVATE_INCLUDES =  \
	STARMAN_INC ST_LIMITS_INC ST_IMAGE_INC ST_TABLE_INC \
	ST_GRAPH_INC ST_CHOICE_INC ST_DS_GEN_INC ST_DS_PANEL_INC \
	addstars.inc air.inc aper.inc apermag.inc \
	automag.inc average.inc chi.inc diagram.inc \
	dsx_panel.inc \
	dustring.inc gaumag.inc gposns.inc \
	imcalc.inc imcube.inc imcut.inc imdes.inc imfits_dr.inc \
	imjoin.inc imkey.inc imtype.inc inspect.inc \
	interact.inc alter.inc alter_calc.inc lut.inc meas_fit.inc \
	measure.inc profile.inc profw.inc \
	scrut.inc simplemag.inc sprinkle.inc starfind.inc \
	starflib_adam.inc tbcalc.inc tbchart.inc tbcomps.inc \
	tbdes.inc tbkey.inc tblist.inc tbnmatch.inc \
	tbplot.inc tbsheet.inc tbstat.inc tbtran_auto.inc \
	unccd.inc \
	virt.inc x_main.inc \
	ds_gen.h   dsx_gen.h  image.h ds_lut.h \
	dsx_lut.h    lut.h  ds_panel.h   dsx_panel.h  virt.h

DATAFILES = data/addstars_in.sdf data/addstars_instars.sdf	      \
data/addstars_profile.sdf data/automag_in.sdf data/average_in1.sdf    \
data/average_in2.sdf data/average_inchi1.sdf data/average_inchi2.sdf  \
data/chi_in.sdf data/diagram_in2.sdf data/diagram_in3.sdf	      \
data/dustring_in.sdf data/galaxy.sdf data/imcalc_ina.sdf	      \
data/imcalc_inb.sdf data/imcube_in.sdf data/imcut_in.sdf	      \
data/imdes_in.sdf data/imfits_dr_in data/imflash_in.sdf		      \
data/imjoin_in1.sdf data/imjoin_in2.sdf data/imkey_in.sdf	      \
data/import_in.sdf data/imrotate_in.sdf data/imsmooth_in.sdf	      \
data/imstat_in.sdf data/imtype_in.sdf data/imweed_in.sdf	      \
data/interact_in.sdf data/mag_stars_v.sdf data/measure_in.sdf	      \
data/measure_instars.sdf data/measure_profile.sdf data/profile_in.sdf \
data/profile_instars.sdf data/radius_in.sdf data/simplemag_in.sdf     \
data/simplemag_instars.sdf data/sprinkle_cluster.sdf		      \
data/starfind_in.sdf data/stars_r_rotated.sdf data/stars_v.sdf	      \
data/tbcalc_ina.sdf data/tbcalc_inb.sdf data/tbchart_in.sdf	      \
data/tbcomps_in.sdf data/tbcut_in.sdf data/tbdes_in.sdf		      \
data/tbjoin_in1.sdf data/tbjoin_in2.sdf data/tblist_in.sdf	      \
data/tbload_in data/tbmatch_in1.sdf data/tbmatch_in2.sdf	      \
data/tbnative_in.sdf data/tbnmatch_in1.sdf data/tbnmatch_in2.sdf      \
data/tbnmatch_in3.sdf data/tbplot_im.sdf data/tbplot_in1.sdf	      \
data/tbplot_in2.sdf data/tbpmatch_in1.sdf data/tbpmatch_in2.sdf	      \
data/tbrenum_in.sdf data/tbsheet_in.sdf data/tbsort_in.sdf	      \
data/tbstat_in1.sdf data/tbstat_in2.sdf data/tbtran_auto_in.sdf	      \
data/tbtran_auto_master.sdf data/tbtran_do_in.sdf		      \
data/tbtran_make_in1.sdf data/tbtran_make_in2.sdf data/tbvalue_in.sdf \
data/tbweed_in.sdf data/unccd_in.sdf


#  Generate ifl and startup scripts from the ifd file and then do any
#  necessary post processing of the generated files to overcome
#  deficiencies in ifd2star
$(IFL_FILES) $(DYNSCRIPTS): starman.ifd
	$(IFD2STAR) starman
