## Process this file with automake to produce Makefile.in

# grfont.dat and rgb.txt are not actually programs but they still need to
# go into bin/

bin_PROGRAMS = pgdisp
pgdisp_SOURCES = $(C_ROUTINES)
pgdisp_LDADD = $(X_LIBS) $(X_EXTRA_LIBS) $(X_PRE_LIBS) -lX11
dist_noinst_HEADERS = $(PRIVATE_INCLUDES)
dist_noinst_DATA = figdisp.icon nocursor.icon
AM_CFLAGS = -DPGDISP

C_ROUTINES = cleanup.c getcolors.c handlexevent.c initlock.c ntoh.c    \
resdb.c updatelgtitle.c exposelgwin.c getdata.c initlgluts.c	       \
initwmattr.c pgdisp.c resizelgwin.c waitevent.c figcurs.c getvisuals.c \
initlgwin.c mainloop.c proccom.c returnbuf.c

PRIVATE_INCLUDES = commands.h  figdisp.h  globals.h  messages.h

