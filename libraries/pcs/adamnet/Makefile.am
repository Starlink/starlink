## Process this file with automake to produce Makefile.in

bin_PROGRAMS = adamnet

adamnet_SOURCES = \
	$(C_ROUTINES) \
	$(PRIVATE_INCLUDES)

LDADD = -lsock -lmsp

C_ROUTINES = ant.c adamnet.c

noinst_MESSAGES = ant_err.h
PRIVATE_INCLUDES = ant_par.h ant_sys.h ant_cmn.h \
                   ant.h ant_static.h $(noinst_MESSAGES)

BUILT_SOURCES = $(noinst_MESSAGES)

# have no idea whether these programs are useful
noinst_PROGRAMS = traceit antmaster antslave

traceit_SOURCES = traceit.c
traceit_LDADD = `ams_link_adam`

antmaster_SOURCES = antmaster.c
antmaster_LDADD = `ams_link_adam`

antslave_SOURCES = antslave.c
antslave_LDADD = `ams_link_adam`
