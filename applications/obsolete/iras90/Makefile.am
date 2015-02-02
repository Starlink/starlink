## Process this file with automake to produce Makefile.in

bin_SCRIPTS = $(DYNSCRIPTS) misc/pscont
bin_PROGRAMS = ffield
ffield_SOURCES = misc/ffield.f

bin_MONOLITHS = iras90_pm
iras90_pm_SOURCES = iras90_pm.f
iras90_pm_TASKS = backcrdd coaddcrdd colcorr coltemp deglcrdd destcrdd \
findcrdd history i90help ifield irastrace makeprof makeresp mapcrdd    \
newunits pointcrdd poscrdd prepare qualtobad remqual setimage setqual  \
showqual simcrdd skyalign skybox skygrid skyline skymark skyphot       \
skypos skywrite tracecrdd

iras90_pm_LDADD = apps/libapps.la irm/libirm.la irq/libirq.la iri/libiri.la irc/libirc.la ira/libira.la i90/libi90.la \
	`ard_link_adam` `agi_link_adam` `fio_link_adam` \
	`snx_link_adam` `ndf_link_adam` `ary_link_adam` \
	`sla_link_adam` `shl_link_adam` `ndg_link_adam`

IFL_FILES = $(iras90_pm_TASKS:=.ifl)

pkgdata_DATA = iras90.news

# Data files are installed into bindir
mydatadir = $(bindir)
dist_mydata_DATA = $(PSF_FILES) $(DATA_FILES)

DYNSCRIPTS = iras90.csh iras90.sh iras90.icl

PRIVATE_INCLUDES = ficomm tra_com sky_par

#  Generate ifl and startup scripts from the ifd file
$(IFL_FILES) $(DYNSCRIPTS): iras90.ifd
	$(IFD2STAR) iras90

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES) iras90_pm.ifl

#  Detector PSF images.

PSF_FILES = data/psf1.sdf data/psf2.sdf data/psf3.sdf data/psf4.sdf   \
data/psf5.sdf data/psf6.sdf data/psf7.sdf data/psf8.sdf data/psf9.sdf \
data/psf10.sdf data/psf11.sdf data/psf12.sdf data/psf13.sdf	      \
data/psf14.sdf data/psf15.sdf data/psf16.sdf data/psf18.sdf	      \
data/psf19.sdf data/psf21.sdf data/psf22.sdf data/psf23.sdf	      \
data/psf24.sdf data/psf25.sdf data/psf26.sdf data/psf27.sdf	      \
data/psf28.sdf data/psf29.sdf data/psf30.sdf data/psf31.sdf	      \
data/psf32.sdf data/psf33.sdf data/psf34.sdf data/psf35.sdf	      \
data/psf37.sdf data/psf38.sdf data/psf39.sdf data/psf40.sdf	      \
data/psf41.sdf data/psf42.sdf data/psf43.sdf data/psf44.sdf	      \
data/psf45.sdf data/psf46.sdf data/psf47.sdf data/psf48.sdf	      \
data/psf49.sdf data/psf50.sdf data/psf51.sdf data/psf52.sdf	      \
data/psf53.sdf data/psf54.sdf data/psf55.sdf data/psf56.sdf	      \
data/psf57.sdf data/psf58.sdf data/psf59.sdf data/psf60.sdf	      \
data/psf61.sdf data/psf62.sdf data/newspfarch.sdf data/profiles.sdf

#  Spectral response NDFs.

DATA_FILES = specresp12.sdf specresp25.sdf \
specresp60.sdf specresp100.sdf

# Makeresp needs to be done as a soft link prior to install
makeresp: iras90_pm
	$(LN_S) iras90_pm makeresp


# DATA files are generated
specresp12.sdf: makeresp makeresp.ifc
	./makeresp specresp12 12

specresp25.sdf: makeresp makeresp.ifc
	./makeresp specresp25 25

specresp60.sdf: makeresp makeresp.ifc
	./makeresp specresp60 60

specresp100.sdf: makeresp makeresp.ifc
	./makeresp specresp100 100

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
    sun161_a_1_1.pdf sun161_a_3_1_canon.dat sun161_a_4_3.pdf sun161_a_1_2.pdf \
    sun161_a_4_1.pdf sun161_a_4_4.pdf sun161_a_2_1.pdf sun161_a_4_2.pdf \
    sun161_a_4_5.pdf

# Define the help source file.
starhelp_DATA = iras90_help.shl

.hlp.shl:
	@HLIB@ $<


# Fixup scripts with bin location
# The scripts needs to have installation locations edited into them
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@pgplotdir\@,$(exec_prefix)/bin,g' \
	-e 's,@PACKAGE_VERSION\@,$(VERSION),g'

echwind.csh: echwind.csh.in
	-rm -f echwind.csh.tmp echwind.csh
	$(edit) \
	    $(srcdir)/echwind.csh.in > echwind.csh.tmp
	mv echwind.csh.tmp echwind.csh

SUBDIRS = i90 ira irc iri irq irm apps
