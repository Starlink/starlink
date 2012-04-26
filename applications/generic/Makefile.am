## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = dcv_link dcv_link_adam generic

lib_LTLIBRARIES = libdcv.la
nodist_libdcv_la_SOURCES = dcv_ttoub.f dcv_ttouw.f
EXTRA_libdcv_la_SOURCES = DCV_FUN.F dcv_ttoub.gen dcv_ttouw.gen
include_HEADERS = DCV_EXT DCV_PAR cgeneric.h cgeneric_defs.h
nodist_include_HEADERS = DCV_FUN

dist_staretc_DATA = $(AUX_FILES)

dist_starnews_DATA = generic.news
dist_pkgdata_DATA = LICENCE

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  List of package auxiliary files.  These are text files uesd by GENERIC.
AUX_FILES = byte byte_asterix \
	character character_asterix \
	double double_asterix \
	integer integer8 integer_asterix \
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

# Run a .F file through the fpp preprocessor, to produce a file with no
# The following deals with case-insensitive filesystems, on which
# foo.f and foo.F would be the same file.  FPP_OUTPUT is
# either "" (in which case the preprocessor writes to foo.f, and
# the filesystem is presumably case-sensitive) or ">$@".
DCV_FUN: DCV_FUN.F
	rm -f DCV_FUN
	$(FPP) $(FPP_MAKE_FLAGS) -I. DCV_FUN.F $(FPP_OUTPUT)
	test -f DCV_FUN || mv DCV_FUN.f DCV_FUN

BUILT_SOURCES = DCV_FUN

# fgeneric needs to be made as a soft link to generic on install.
install-exec-local:
	cd $(DESTDIR)$(bindir); \
	rm -f fgeneric; \
	echo $(LN_S) generic fgeneric; \
	$(LN_S) generic fgeneric; \
	$(MANIFEST) && echo "MANIFEST:$(DESTDIR)$(bindir)/fgeneric" || :

CLEANFILES = dcv_ttoub.f dcv_ttouw.f DCV_FUN

# Simple test of C generics.
TESTS = generic_main
check_PROGRAMS = generic_main
generic_main_SOURCES = generic_main.c generic_func.cgen
