## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = sgs_link_adam

lib_LTLIBRARIES = libsgspar_adam.la

libsgspar_adam_la_SOURCES = $(F_ROUTINES) $(BLOCKDATA_ROUTINES) \
        $(PRIVATE_INCLUDES)

F_ROUTINES = sgs1_activ.f sgs1_chkzn.f sgs1_getzd.f sgs_assoc.f \
sgs_deact.f  sgs1_fndzd.f sgs_annul.f  sgs_cancl.f


PRIVATE_INCLUDES =  sgsenv_par sgsgo_cmn sgspa_cmn

BLOCKDATA_ROUTINES = sgs1_blk.f


# Testing


# Testing
# Need to know how to build adam applications
                                                                                
# sgs_test is ADAM monolith

TESTS = sgspar_test sgs_test

check_PROGRAMS = sgspar_test
check_MONOLITHS = sgs_test

# Not sure why pthread is needed on linux. Need to investigate
# no variable sgs_test_TASKS -- sgs_test is an ATASK
sgs_test_SOURCES = sgs_test.f
sgs_test_LDADD = $(LDADD) `$(srcdir)/sgs_link_adam` `par_link_adam` -lpthread

sgspar_test_SOURCES = sgspar_test.f
sgspar_test_LDADD = libsgspar_adam.la `$(srcdir)/sgs_link_adam` `par_link_adam`

CLEANFILES = sgs_test sgs_test.ifc


