dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    Named chart so that staretcdir is defined properly
AC_INIT(chart, 4.0-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([tconv.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Open specifiers
AC_FC_OPEN_SPECIFIERS(readonly)
AC_FC_OPEN_SPECIFIERS(carriagecontrol='LIST')

dnl    What unit does OPEN's recl specifier assume?
AC_FC_RECL_UNIT

AC_CONFIG_FILES(Makefile)

AC_OUTPUT
