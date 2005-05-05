## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = do-xxx cmp-xxx extmk extmakefile \
                   $(DO_FILTERS) $(CMP_FILTERS) \
                   extreme.csh

bin_PROGRAMS = $(FILTERS)

include_HEADERS = EXT_DEC EXT_DEF EXT_PAR extreme.h

noinst_HEADERS = ctokens.h ftokens.h ygen.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

TESTS = extreme_test.sh

#  List of source code filter programs.

FILTERS = inscnf frepint crepint
DO_FILTERS = do-inscnf do-frepint do-crepint
CMP_FILTERS = cmp-inscnf cmp-frepint cmp-crepint

crepint_SOURCES = crepint.c ctok.l ygen.c
frepint_SOURCES = frepint.c ftok.l ygen.c
inscnf_SOURCES = inscnf.c ftok.l ygen.c


$(DO_FILTERS): do-xxx
	cp do-xxx $@

$(CMP_FILTERS): cmp-xxx
	cp cmp-xxx $@

ssn73.tex: ssn73-pre.tex
	$(srcdir)/mkdoc 

EXTRA_DIST = ssn73-pre.tex extreme_test.sh

