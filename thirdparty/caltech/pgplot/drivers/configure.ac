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
dnl   In true autoconf style, we do not attempt to generate this list
dnl   automatically (see the automake manual's FAQ for justification).
dnl
dnl   The common_built_drivers are common to the standard set and the
dnl   Starlink set.

common_built_drivers="exdriv.lo pgdriv.lo cadriv.lo hgdriv.lo lxdriv.lo ppdriv.lo vtdriv.lo gidriv.lo hidriv.lo psdriv.lo ttdriv.lo cwdriv.lo gldriv.lo nedriv.lo wddriv.lo hpdriv.lo lsdriv.lo nudriv.lo qmdriv.lo vadriv.lo cgdriv.lo nexsup.lo rvdriv.lo x2driv.lo xmdriv.lo figdisp_comm.lo pgxwin.lo tkdriv.lo xadriv.lo"

dnl   Add the png driver if the PNG header and library are available.
AC_CHECK_HEADER(png.h,
                AC_CHECK_LIB(png, png_info_init,
                             [LIBS="$LIBS -lpng";BUILT_DRIVERS="$BUILT_DRIVERS pndriv.lo"],
                             [AC_MSG_WARN(PNG libraries not found)],
                             [-lz]))

BUILT_DRIVERS="$common_built_drivers xwdriv.lo"
STAR_BUILT_DRIVERS="$common_built_drivers star_xwdriv.lo gwmdriv.lo"

AC_SUBST(BUILT_DRIVERS)
AC_SUBST(STAR_BUILT_DRIVERS)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile pgplot_link pgplot_link_adam])

dnl    This is the bit that does the actual work
AC_OUTPUT
