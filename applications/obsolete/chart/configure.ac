dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(chart, 4.0-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([chart_mon.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CNF_COMPATIBLE_SYMBOLS

STAR_CHECK_PROGS(ifd2star)
STAR_CHECK_PROGS(hlib)

dnl    Look for NAG
AC_LANG_PUSH(Fortran)
AC_CHECK_LIB(nag, m01dbf)
AC_LANG_POP(Fortran)


dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [chr err sgs gks])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun32)

AC_CONFIG_SUBDIRS([libchart applic])

AC_CONFIG_FILES(Makefile component.xml chart.ifd)

AC_OUTPUT
