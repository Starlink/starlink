## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = gks_link_adam gks1_link_adam

lib_LTLIBRARIES = libgkspar_adam.la

libgkspar_adam_la_SOURCES = $(F_ROUTINES) $(BLOCKDATA_ROUTINES) \
        $(PRIVATE_INCLUDES)

F_ROUTINES = gks1_activ.f gks1_chkid.f gks1_getgd.f gks_assoc.f	       \
gks_gstat.f gks1_ass.f gks1_deas.f gks1_stop.f gks_cancl.f gks_reset.f \
gks1_fndgd.f gks_annul.f gks_deact.f

PRIVATE_INCLUDES = gksenv_par gksgo_cmn gkspa_cmn

BLOCKDATA_ROUTINES = gks1_blk.f


# Testing
# Need to know how to build adam applications

# gks_test is ADAM monolith

TESTS = gkspar_test gks_test

check_PROGRAMS = gkspar_test
check_MONOLITHS = gks_test

# Not sure why pthread is needed on linux. Need to investigate
# no variable gks_test_TASKS -- gks_test is an ATASK
gks_test_SOURCES = gks_test.f
gks_test_LDADD = $(LDADD) `$(srcdir)/gks_link_adam` `par_link_adam` -lpthread


gkspar_test_SOURCES = gkspar_test.f
gkspar_test_LDADD = libgkspar_adam.la `$(srcdir)/gks_link_adam` `par_link_adam` `err_link_adam`


CLEANFILES = gks_test gks_test.ifc


