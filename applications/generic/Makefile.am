## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = dcv_link dcv_link_adam generic

lib_LTLIBRARIES = libdcv.la
nodist_libdcv_la_SOURCES = dcv_ttoub.f dcv_ttouw.f

include_HEADERS = DCV_EXT DCV_PAR
nodist_include_HEADERS = DCV_FUN

staretc_DATA = $(AUX_FILES)

#  List of package auxilliary files.  These are text files uesd by GENERIC.
AUX_FILES = byte byte_asterix \
	character character_asterix \
	double double_asterix \
	integer integer_asterix \
	logical logical_asterix \
	real real_asterix \
	ubyte ubyte_asterix \
	uword uword_asterix \
	word word_asterix \
	wrap72


dcv_ttoub.f: dcv_ttoub.gen
	env GENERIC_DIR=. ./generic -t bdirwW dcv_ttoub.gen
dcv_ttouw.f: dcv_ttouw.gen
	env GENERIC_DIR=. ./generic -t bBdirw dcv_ttouw.gen
## DCV_FUN: DCV_FUN.f
## 	cp DCV_FUN.f DCV_FUN
## DCV_FUN.f: DCV_FUN.fpp
DCV_FUN: DCV_FUN.fpp
	$(FPP) $(FPPFLAGS) $(CPPFLAGS) -I. DCV_FUN.fpp $(FPP_OUTPUT)

BUILT_SOURCES = DCV_FUN
