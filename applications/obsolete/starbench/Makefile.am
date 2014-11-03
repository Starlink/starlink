## Process this file with automake to produce Makefile.in

SUBDIRS = slatest

bin_SCRIPTS = bench scan submit $(STARTUP_SCRIPT) $(PRIVATESCRIPTS) \
	$(PRIVATE_PLATFORMSCRIPTS)

bin_PROGRAMS = bch_cfp bch_sysinfo

bin_MONOLITHS = bch_fft
bch_fft_SOURCES = bch_fft.f $(GEN_F_ROUTINES)
bch_fft_LDADD = `$(srcdir)/starbench_link_adam`

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@


bch_cfp_SOURCES = bch_cfp.c
bch_cfp_LDADD = slatest/libsbm.la -lm

bch_sysinfo_SOURCES = sysinfo.c

dist_pkgdata_DATA = $(DATA_FILES)

#

STARTUP_SCRIPT = starbench.csh starbench.sh

IFD_FILES = starbench.ifd
IFL_FILES = bch_fft.ifl
IFC_FILES = $(IFL_FILES:.ifl=.ifc)

KPS_DR_GEN = kps1_fopr.gdr kps1_fofo.gdr

GEN_F_ROUTINES = $(KPS_DR_GEN:.gdr=.f)

# Public scripts

#  List of private script files.  These are scripts which are required
#  by the public scripts but will not be invoked directly by users.
#  They will be installed in the $(INSTALL_BIN) directory with execute
#  permission set.

PRIVATESCRIPTS = bch_ccdpk bch_ccdred bch_daophot bch_full.prl bch_head.awk \
bch_images bch_imark.prl bch_kappa bch_load.awk bch_pisa \
bch_print.awk bch_result.awk bch_scan.awk bch_smark.prl bch_specdre \
login.cl mkiraf bch_ckver.awk \
tmpfile


PRIVATE_PLATFORMSCRIPTS = bch_info

#  List of ancilliary data files.

DATA_FILES = bch_ccdpk.chk bch_ccdred.chk bch_daophot.chk bch_images.chk \
bch_kappa.chk bch_pisa.chk bch_specdre.chk datcentes.par \
datdataps.par datfitsks.par datphotps.par starbench.cal

.gdr.f:
	$(FGENERIC)  -t dr $<

$(IFL_FILES) $(STARTUP_SCRIPT): $(IFD_FILES)
	$(IFD2STAR) starbench

