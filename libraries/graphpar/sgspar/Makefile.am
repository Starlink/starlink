## Process this file with automake to produce Makefile.in

bin_SCRIPTS = sgs_link_adam

lib_LTLIBRARIES = libsgspar_adam.la

libsgspar_adam_la_SOURCES = $(F_ROUTINES) $(BLOCKDATA_ROUTINES) \
        $(PRIVATE_INCLUDES)

F_ROUTINES = sgs1_activ.f sgs1_chkzn.f sgs1_getzd.f sgs_assoc.f \
sgs_deact.f  sgs1_fndzd.f sgs_annul.f  sgs_cancl.f


PRIVATE_INCLUDES =  sgsenv_par sgsgo_cmn sgspa_cmn

BLOCKDATA_ROUTINES = sgs1_blk.f


# Testing
# Need to know how to build adam applications
