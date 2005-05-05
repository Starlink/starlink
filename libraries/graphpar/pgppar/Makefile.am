## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = pgp_link_adam

lib_LTLIBRARIES = libpgppar_adam.la

libpgppar_adam_la_SOURCES = $(F_ROUTINES) $(BLOCKDATA_ROUTINES) \
        $(PRIVATE_INCLUDES)

F_ROUTINES = pgp1_activ.f pgp1_chkun.f pgp1_getud.f pgp_assoc.f \
pgp_deact.f  pgp1_fndud.f pgp_annul.f  pgp_cancl.f

PRIVATE_INCLUDES = pgpenv_par pgpgo_cmn pgppa_cmn

BLOCKDATA_ROUTINES = pgp1_blk.f

# Testing

# pgp_test is ADAM monolith

TESTS = pgppar_test pgp_test

check_PROGRAMS = pgppar_test
check_MONOLITHS = pgp_test

# Not sure why pthread is needed on linux. Need to investigate
# no variable pgp_test_TASKS -- pgp_test is an ATASK
pgp_test_SOURCES = pgp_test.f
pgp_test_LDADD = $(LDADD) `$(srcdir)/pgp_link_adam` `par_link_adam` -lpthread

pgppar_test_SOURCES = pgppar_test.f
pgppar_test_LDADD = libpgppar_adam.la `$(srcdir)/pgp_link_adam` `par_link_adam`

CLEANFILES = pgp_test pgp_test.ifc


