dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(pgplot-drivers, 5.2.2-1, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([bcdriv.f])



 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

AC_C_BIGENDIAN
AC_HEADER_STDC
AC_PATH_XTRA

dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS

AC_CONFIG_SUBDIRS([xathena xtk xmotif])

dnl   These are the driver files that will actually be built.
dnl   The intention is for autoconf to generate this list automatically.
dnl   For now we kluge and hardwire the driver list. Remove items that should
dnl   not be built.

BUILT_DRIVERS="exdriv.lo pgdriv.lo cadriv.lo hgdriv.lo lxdriv.lo ppdriv.lo vtdriv.lo gidriv.lo hidriv.lo psdriv.lo ttdriv.lo cwdriv.lo gldriv.lo nedriv.lo wddriv.lo hpdriv.lo lsdriv.lo nudriv.lo qmdriv.lo vadriv.lo cgdriv.lo nexsup.lo rvdriv.lo x2driv.lo xmdriv.lo figdisp_comm.lo pgxwin.lo pndriv.lo tkdriv.lo xadriv.lo xwdriv.lo"

STAR_BUILT_DRIVERS="exdriv.lo pgdriv.lo cadriv.lo hgdriv.lo lxdriv.lo ppdriv.lo vtdriv.lo gidriv.lo hidriv.lo psdriv.lo ttdriv.lo cwdriv.lo gldriv.lo nedriv.lo wddriv.lo hpdriv.lo lsdriv.lo nudriv.lo qmdriv.lo vadriv.lo cgdriv.lo nexsup.lo rvdriv.lo x2driv.lo xmdriv.lo figdisp_comm.lo pgxwin.lo pndriv.lo tkdriv.lo xadriv.lo star_xwdriv.lo gwmdriv.lo"

AC_SUBST(BUILT_DRIVERS)
AC_SUBST(STAR_BUILT_DRIVERS)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
