## Process this file with automake to produce Makefile.in

include $(srcdir)/Makefile.starconf

bin_PROGRAMS = adamnet

adamnet_SOURCES = \
	$(C_ROUTINES) \
	$(PRIVATE_INCLUDES)

LDADD = -lsock -lmsp

C_ROUTINES = ant.c adamnet.c

PRIVATE_INCLUDES = ant_par.h ant_err.h ant_sys.h ant_cmn.h \
                   ant.h ant_static.h

BUILT_SOURCES = ant_err.h @BUILT_SOURCES@
