dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(convert, 1.5-4, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([convert.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS

dnl    Progs needed for gif2ndf, ndf2gif and tiff2ndf
AC_CHECK_PROGS(ppmtogif)
AC_CHECK_PROGS(giftopnm)
AC_CHECK_PROGS(ppmtopgm)
AC_CHECK_PROGS(pnmflip)
AC_CHECK_PROGS(pnmtofits)
AC_CHECK_PROGS(tifftopnm)

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [figaro ast sae chr hds fio cnf grp mers prm ndf ndg par psx subpar])

STAR_LATEX_DOCUMENTATION([sun55])

AC_CONFIG_SUBDIRS([aif cof coi con idl iraf])

AC_CONFIG_FILES([Makefile component.xml ])

AC_OUTPUT
