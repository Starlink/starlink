## Process this file with automake to produce Makefile.in

# grfont.dat and rgb.txt are not actually programs but they still need to
# go into bin/

noinst_LIBRARIES = libpgplot_drivers.a
libpgplot_drivers_a_SOURCES = $(F_ROUTINES) $C(ROUTINES)

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately). Keep this list in alphabetical
#  order so that the subroutine description html comes out in the right 
#  order.

F_ROUTINES = bcdriv.f exdriv.f gvdriv.f ladriv.f lvdriv.f pgdriv.f \
tfdriv.f vbdriv.f xedriv.f cadriv.f gcdriv.f hgdriv.f lhdriv.f	   \
lxdriv.f ppdriv.f todriv.f vtdriv.f zedriv.f ccdriv.f gidriv.f	   \
hidriv.f ljdriv.f mfdriv.f psdriv.f ttdriv.f cwdriv.f \
gldriv.f hjdriv.f lndriv.f nedriv.f pxdriv.f txdriv.f wddriv.f	   \
epdriv.f godriv.f hpdriv.f lsdriv.f nudriv.f qmdriv.f vadriv.f	   \
wsdriv.f

C_ROUTINES = cgdriv.c nexsup.c pgxwin_server.c rvdriv.c x2driv.c     \
xmdriv.c figdisp_comm.c pgxwin.c pndriv.c tkdriv.c xadriv.c xwdriv.c

AM_CFLAGS = -DPG_PPU
