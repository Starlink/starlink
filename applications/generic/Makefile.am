## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = generic

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

# fgeneric needs to be made as a soft link to generic on install.
install-exec-local:
	cd $(DESTDIR)$(bindir); \
	rm -f fgeneric; \
	echo $(LN_S) generic fgeneric; \
	$(LN_S) generic fgeneric; \
	$(MANIFEST) && echo "MANIFEST:$(DESTDIR)$(bindir)/fgeneric" || :

# Simple test of C generics.
TESTS = generic_main
check_PROGRAMS = generic_main
generic_main_SOURCES = generic_main.c generic_func.cgen
