#
# RAL GKS ww bits for X Workstation Driver Makefile.
#
# ---- copyright (c) Mark M Martin. RAL. 1988
#
# 
# Maintenance Log:
#
#   ??/01/88   MMM  ww makefile for X device dependent stuff.
#   ??/??/88   TAW  Adapted for RAL GKS X driver.
#   04/07/89   PLP  Adapted for inclusion into MS revision 1.30. 
#                   Switched to X revision 3. Cut out obsolete 
#                   (well, hopefully) code. libXww.a not cleaned.
#   11/07/89   PLP  Nothing is cleaned here - moved to X/Makefile.
#   12/07/89   PLP  Removed libXww.a - everything added to libgks now.

SYSTEM = alpha_OSF1
RLSE = 1.37
XBIN = /usr/ral/bin
XLINT = `-lX`
CFLAGS = -O -std1

SOURCES=	\
	gk0xbmbox.c \
	gk0xbmcopy.c \
	gk0xbmget.c \
	gk0xcolour.c \
	gk0xcursor.c \
	gk0xfont.c \
	gk0xget.c \
	gk0xgetwin.c \
	gk0xinput.c \
	gk0xline.c \
	gk0xmap.c \
	gk0xstack.c \
	gk0xutil.c
#	bmslide.c
#	fnbox.c
#	stream.c
#	screen.c 
#	share.c 
COMSOURCES=	\
	../com/gk0xxbmbox.c \
	../com/gk0xxbmcopy.c \
	../com/gk0xxbmget.c \
	../com/gk0xxcursor.c \
	../com/gk0xxfont.c \
	../com/gk0xxgetwin.c \
	../com/xglobals.c \
	../com/gk0xxinput.c \
	../com/gk0xxline.c \
	../com/gk0xxmap.c \
	../com/gk0xxstack.c \
	../com/gk0xxstream.c \
	../com/gk0xxutil.c
#	../com/xcolour.c
##	../com/xbmslide.c 
##	../com/xfnbox.c 
##	../com/xget.c 
DISOURCES=	\
	../../di/gk0xzbox.c \
	../../di/gk0xzcircle.c \
	../../di/gk0xzcudecode.c \
	../../di/gk0xzdecode.c \
	../../di/gk0xzemu.c \
	../../di/gk0xzemusel.c \
	../../di/gk0xzgrey.c 
##	../../di/zabsrel.c 
##	../../di/zscandir.c 
##	../../di/ztreecache.c 
##	../../di/zbezier.c 
##	../../di/zcuencode.c 
##	../../di/zencode.c 
##	../../di/zellipse.c 
##	../../di/zemucurse.c 
##	../../di/zexrep.c 
##	../../di/zfill.c 
##	../../di/zoutline.c 
##	../../di/zpopup.c 
##	../../di/zpull.c 
##	../../di/zscreen.c 
##	../../di/ztext.c 
##	../../di/ztext2.c 
##	../../di/ztext3.c 
##	../../di/ztree.c 
##	../../di/zutil.c 
##	../../di/zwordpop.c
ALLSOURCES= $(SOURCES) $(COMSOURCES) $(DISOURCES)
OBJECTS=	\
	gk0xbmbox.o \
	gk0xbmcopy.o \
	gk0xbmget.o \
	gk0xcolour.o \
	gk0xcursor.o \
	gk0xfont.o \
	gk0xget.o \
	gk0xgetwin.o \
	gk0xinput.o \
	gk0xline.o \
	gk0xmap.o \
	gk0xstack.o \
	gk0xutil.o
#	bmslide.o
#	fnbox.o
#	stream.o
##	screen.o 
##	share.o 
COMOBJECTS=	\
	gk0xxbmbox.o \
	gk0xxbmcopy.o \
	gk0xxbmget.o \
	gk0xxcursor.o \
	gk0xxfont.o \
	gk0xxgetwin.o \
	gk0xxinput.o \
	gk0xxline.o \
	gk0xxmap.o \
	gk0xxstack.o \
	gk0xxstream.o \
	gk0xxutil.o
#	xcolour.o
##	xbmslide.o 
##	xget.o 
##	xfnbox.o 
DIOBJECTS=	\
	gk0xzbox.o \
	gk0xzcircle.o \
	gk0xzcudecode.o \
	gk0xzdecode.o \
	gk0xzemu.o \
	gk0xzemusel.o \
	gk0xzgrey.o 
##	zabsrel.o 
##	zscandir.o 
##	ztreecache.o 
##	zbezier.o 
##	zcuencode.o 
##	zencode.o 
##	zellipse.o 
##	zemucurse.o 
##	zexrep.o 
##	zfill.o 
##	zoutline.o 
##	zpopup.o 
##	zpull.o 
##	zscreen.o 
##	ztext.o 
##	ztext2.o 
##	ztext3.o 
##	ztree.o 
##	zutil.o 
##	zwordpop.o
GLOBALS=gk0xxglobals.o
ALLOBJECTS=$(OBJECTS) $(COMOBJECTS) $(DIOBJECTS) 
DIDEPENDS=	\
	../../di/wwinfo.h \
	../../di/di.h
DEPENDS=	\
	$(DIDEPENDS) \
	../xdd.h \
	dd.h


$(SYSTEM):	sccs $(ALLOBJECTS) $(GLOBALS)
		$(AR) ../../../../../lib/$(SYSTEM)/libgks-$(RLSE).a \
$(ALLOBJECTS) $(GLOBALS)

# Get files from under SCCS.
sccs:
		-sccs get -s -r$(RLSE) SCCS

# Redefine the rule for building c objects (to extend the options)
.c.o:
		$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c $<

# clean
clean:
		-rm -f *.o
		sccs clean


# ---- machine specific source code. some files may be empty

gk0xbmbox.o:	gk0xbmbox.c	$(DEPENDS)
gk0xbmcopy.o:	gk0xbmcopy.c	$(DEPENDS)
gk0xbmget.o:	gk0xbmget.c	$(DEPENDS)
#bmslide.o:	bmslide.c	$(DEPENDS)
#stream.o:	stream.c	$(DEPENDS)
#fnbox.o:	fnbox.c		$(DEPENDS)
gk0xfont.o:	gk0xfont.c	$(DEPENDS) font.h
gk0xcursor.o:	gk0xcursor.c	$(DEPENDS)
gk0xinput.o:	gk0xinput.c	$(DEPENDS)
gk0xline.o:	gk0xline.c	$(DEPENDS)
gk0xmap.o:	gk0xmap.c	$(DEPENDS) map.h
gk0xscreen.o:	gk0xscreen.c	$(DEPENDS)
gk0xshare.o:	gk0xshare.c	$(DEPENDS)
gk0xstack.o:	gk0xstack.c	$(DEPENDS)
gk0xutil.o:	gk0xutil.c	$(DEPENDS)
gk0xget.o:	gk0xget.c	$(DEPENDS)
gk0xgetwin.o:	gk0xgetwin.c	$(DEPENDS)
gk0xcolour.o:	gk0xcolour.c	$(DEPENDS)

# ---- common source code dependent on machine specific junk structures.
# ---- -I. to use this directory's dd.h file.

gk0xxbmbox.o:	../com/gk0xxbmbox.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxbmbox.c
gk0xxbmcopy.o:	../com/gk0xxbmcopy.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxbmcopy.c
gk0xxbmget.o:	../com/gk0xxbmget.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxbmget.c
gk0xxbmslide.o:	../com/gk0xxbmslide.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxbmslide.c
gk0xxstream.o:	../com/gk0xxstream.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxstream.c
gk0xxfnbox.o:	../com/gk0xxfnbox.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxfnbox.c
gk0xxfont.o:	../com/gk0xxfont.c	$(DEPENDS) font.h
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxfont.c
gk0xxglobals.o:	../com/gk0xxglobals.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxglobals.c
gk0xxcursor.o:	../com/gk0xxcursor.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxcursor.c
gk0xxinput.o:	../com/gk0xxinput.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxinput.c
gk0xxline.o:	../com/gk0xxline.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxline.c
gk0xxmap.o:	../com/gk0xxmap.c	$(DEPENDS) map.h
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxmap.c
gk0xxstack.o:	../com/gk0xxstack.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxstack.c
gk0xxutil.o:	../com/gk0xxutil.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxutil.c
gk0xxget.o:	../com/gk0xxget.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxget.c
gk0xxgetwin.o:	../com/gk0xxgetwin.c	$(DEPENDS)
		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/gk0xxgetwin.c
#xcolour.o:	../com/xcolour.c	$(DEPENDS)
#		$(CC) $(CFLAGS) -I. `$(XBIN)/xr3 -I` -c ../com/xcolour.c

# ---- device independent sources

gk0xzabsrel.o:	../../di/gk0xzabsrel.c
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzabsrel.c
gk0xzscandir.o:	../../di/gk0xzscandir.c
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzscandir.c
gk0xztreecache.o:	../../di/gk0xztreecache.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xztreecache.c
gk0xzbezier.o:	../../di/gk0xzbezier.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzbezier.c
gk0xzbox.o:	../../di/gk0xzbox.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzbox.c
gk0xzcircle.o:	../../di/gk0xzcircle.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzcircle.c
gk0xzcudecode.o:	../../di/gk0xzcudecode.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzcudecode.c
gk0xzcuencode.o:	../../di/gk0xzcuencode.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzcuencode.c
gk0xzdecode.o:	../../di/gk0xzdecode.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzdecode.c
gk0xzencode.o:	../../di/gk0xzencode.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzencode.c
gk0xzemu.o:	../../di/gk0xzemu.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzemu.c
gk0xzellipse.o:	../../di/gk0xzellipse.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzellipse.c
gk0xzemucurse.o:	../../di/gk0xzemucurse.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzemucurse.c
gk0xzemusel.o:	../../di/gk0xzemusel.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzemusel.c
gk0xzexrep.o:	../../di/gk0xzexrep.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzexrep.c
gk0xzfill.o:	../../di/gk0xzfill.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzfill.c
gk0xzgrey.o:	../../di/gk0xzgrey.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzgrey.c
gk0xzoutline.o:	../../di/gk0xzoutline.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzoutline.c
gk0xzpopup.o:	../../di/gk0xzpopup.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzpopup.c
gk0xzpull.o:	../../di/gk0xzpull.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzpull.c
gk0xzscreen.o:	../../di/gk0xzscreen.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzscreen.c
gk0xztext.o:	../../di/gk0xztext.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xztext.c
gk0xztext2.o:	../../di/gk0xztext2.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xztext2.c
gk0xztext3.o:	../../di/gk0xztext3.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xztext3.c
gk0xztree.o:	../../di/gk0xztree.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xztree.c
gk0xzutil.o:	../../di/gk0xzutil.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzutil.c
gk0xzwordpop.o:	../../di/gk0xzwordpop.c	$(DIDEPENDS)
	$(CC) $(CFLAGS) `$(XBIN)/xr3 -I` -c ../../di/gk0xzwordpop.c

