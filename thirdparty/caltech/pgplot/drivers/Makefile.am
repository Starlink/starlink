## Process this file with automake to produce Makefile.in

# grfont.dat and rgb.txt are not actually programs but they still need to
# go into bin/

noinst_LTLIBRARIES = libpgplot_drivers.la
libpgplot_drivers_la_SOURCES = $(F_ROUTINES) $C(ROUTINES)

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately). Keep this list in alphabetical
#  order so that the subroutine description html comes out in the right 
#  order.

F_ROUTINES = exdriv.f pgdriv.f \
vbdriv.f cadriv.f hgdriv.f lhdriv.f	   \
lxdriv.f ppdriv.f vtdriv.f gidriv.f	   \
hidriv.f psdriv.f ttdriv.f cwdriv.f \
gldriv.f nedriv.f wddriv.f	   \
hpdriv.f lsdriv.f nudriv.f qmdriv.f vadriv.f

C_ROUTINES = cgdriv.c nexsup.c pgxwin_server.c rvdriv.c x2driv.c     \
xmdriv.c figdisp_comm.c pgxwin.c pndriv.c tkdriv.c xadriv.c xwdriv.c

AM_CFLAGS = -DPG_PPU

F_ROUTINES_NOTLINUX = bcdriv.f gvdriv.f ladriv.f lvdriv.f tfdriv.f \
xedriv.f gcdriv.f todriv.f zedriv.f ccdriv.f ljdriv.f \
mfdriv.f hjdriv.f lndriv.f pxdriv.f txdriv.f epdriv.f godriv.f wsdriv.f

## Problems with GRWARN //

## ladriv and lvdriv and tfdriv needs OPEN parameter checks
## gcdriv todriv mfdriv

## xedriv VMS only

## lndriv uses IIAND
