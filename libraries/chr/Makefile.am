## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = chr_link chr_link_adam
include_HEADERS = $(PUBLIC_INCLUDES)

lib_LTLIBRARIES = libchr.la
libchr_la_SOURCES = \
	$(F_ROUTINES) \
	$(C_ROUTINES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES)

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libchr_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libchr_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
endif

include_MESSAGES = CHR_ERR
PRIVATE_INCLUDES = CHR_SYS
PUBLIC_INCLUDES = chr.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

F_ROUTINES = chr_abbrv.f chr_achr.f chr_appnd.f chr_atok.f \
        chr_atom.f  chr_btoi.f  chr_clean.f chr_copy.f  chr_ctoc.f \
        chr_ctod.f  chr_ctoi.f  chr_ctok.f chr_ctol.f  chr_ctor.f  chr_dcwrd.f \
        chr_delim.f chr_dtoan.f chr_dtoc.f  chr_equal.f \
        chr_fandl.f chr_fill.f  chr_find.f  chr_fiwe.f  chr_fiws.f \
        chr_htoi.f  chr_iachr.f chr_index.f chr_inset.f chr_isalf.f \
        chr_isalm.f chr_isdig.f chr_isnam.f chr_itob.f  chr_itoc.f \
        chr_itoh.f  chr_itoo.f  chr_lcase.f chr_ldblk.f chr_len.f \
        chr_linbr.f chr_lower.f chr_ltoc.f  chr_move.f  chr_mtoa.f \
        chr_nth.f   chr_otoi.f  chr_pform.f chr_prefx.f chr_putk.f \
        chr_putc.f  chr_putd.f  chr_puti.f  chr_putl.f  chr_putr.f \
        chr_rjust.f chr_rmblk.f chr_rmchr.f chr_rtoan.f chr_rtoc.f \
        chr_scomp.f chr_simlr.f chr_size.f  chr_skchr.f chr_sort.f \
        chr_swap.f  chr_term.f  chr_tochr.f chr_trchr.f chr_trunc.f \
        chr_ucase.f chr_upper.f chr_wild.f  chr1_wild1.f chr1_wild2.f \
        chr1_wild3.f chr1_wild4.f chr1_wild5.f chr1_wild6.f chr1_wild7.f \
        chr_fparx.f chr_lasto.f

C_ROUTINES = chrAppnd.c chrClean.c chrCtod.c chrCtoi.c chrCtor.c \
        chrFandl.c chrFparx.c chrIsalm.c chrIsnam.c chrItoc.c chrLdblk.c \
	chrLen.c chrPutc.c chrPuti.c chrRmblk.c chrSimlr.c chrSimlrN.c \
	chrUcase.c chr_etom.c chr_ktoc.c chr_mtoe.c chrFill.c chrSizetoc.c

BUILT_SOURCES = $(include_MESSAGES)

TEST_ROUTINES_F = case.f compare.f decode.f edit.f \
        encode.f enquire.f port.f search.f \
        test_abbrv.f test_achr.f test_appnd.f test_atok.f \
        test_btoi.f  test_clean.f test_copy.f  test_ctoc.f \
        test_ctod.f  test_ctoi.f  test_ctol.f  test_ctor.f  test_dcwrd.f \
        test_delim.f test_dtoan.f test_dtoc.f  test_equal.f test_etom.f \
        test_fandl.f test_fill.f  test_find.f  test_fiwes.f test_fparx.f \
        test_htoi.f  test_iachr.f test_index.f test_inset.f test_isalf.f \
        test_isalm.f test_isdig.f test_isnam.f test_itob.f  test_itoc.f \
        test_itoh.f  test_itoo.f  test_lcase.f test_ldblk.f test_len.f \
        test_linbr.f test_lower.f test_ltoc.f  test_move.f  \
        test_mtoe.f  test_nth.f   test_otoi.f  test_pform.f test_prefx.f \
        test_putc.f  test_putd.f  test_puti.f  test_putl.f  test_putr.f \
        test_rjust.f test_rmblk.f test_rmchr.f test_rtoan.f test_rtoc.f \
        test_scomp.f test_simlr.f test_size.f  test_skchr.f test_sort.f \
        test_swap.f  test_term.f  test_tochr.f test_trchr.f test_trunc.f \
        test_ucase.f test_upper.f test_wild.f  test_lasto.f \
	test_ctok.f test_ktoc.f

TEST_ROUTINES_C = testAppnd.c testClean.c testCtod.c testCtoi.c \
        testCtor.c testFandl.c testFill.c testFparx.c testIsalm.c \
	testIsnam.c testItoc.c testLdblk.c testLen.c testPutc.c \
	testPuti.c testRmblk.c testSimlr.c testUcase.c

TESTS = chr_test chrTest
check_PROGRAMS = chr_test chrTest

chr_test_SOURCES = chr_test.f $(TEST_ROUTINES_F)
chr_test_LDADD = libchr.la `cnf_link` `starutil_link`

chrTest_SOURCES = chrTest.c $(TEST_ROUTINES_C)
chrTest_LDADD = libchr.la `cnf_link` `starutil_link`

dist_starnews_DATA = chr.news
dist_pkgdata_DATA = LICENCE

EXTRA_DIST = chr_err.msg

CLEANFILES = CHR_ERR





