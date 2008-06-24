AM_CFLAGS = -Wall -Wextra -Wcast-qual -Wcast-align -Wwrite-strings -Wstrict-prototypes -Wmissing-prototypes \      
-Wmissing-declarations -Wredundant-decls -fstack-protector

noinst_LTLIBRARIES = libsc2fts.la

libsc2fts_la_SOURCES = $(C_ROUTINES)

C_ROUTINES =  \
              sc2fts_entry.c sc2fts_common.c \
              sc2fts_addwcs.c sc2fts_groupcoadd.c sc2fts_portimbalance.c \
              sc2fts_freqcorr.c  sc2fts_ifgmflatfield.c  sc2fts_specflatfield.c \
              sc2fts_transcorr.c


noinst_INCLUDES = sc2fts.h sc2fts_entry.h sc2fts_par.h sc2fts_common.h

