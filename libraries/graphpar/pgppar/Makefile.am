## Process this file with automake to produce Makefile.in

bin_SCRIPTS = pgp_link_adam

lib_LTLIBRARIES = libpgppar_adam.la

libpgppar_adam_la_SOURCES = $(F_ROUTINES) $(BLOCKDATA_ROUTINES) \
        $(PRIVATE_INCLUDES) $(PUBLIC_INCLUDES)

F_ROUTINES = pgp1_activ.f pgp1_chkun.f pgp1_getud.f pgp_assoc.f \
pgp_deact.f  pgp1_fndud.f pgp_annul.f  pgp_cancl.f

PUBLIC_INCLUDES = PGP_ERR

PRIVATE_INCLUDES = pgpenv_par pgpgo_cmn pgppa_cmn

BLOCKDATA_ROUTINES = pgp1_blk.f

BUILT_ROUTINES = PGP_ERR

# Testing
# Need to know how to build adam applications
