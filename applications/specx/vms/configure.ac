dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(specx-vms, 6.8-5, t.jenness@jach.hawaii.edu)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([openuf.fpp])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

dnl    Need to know whether we have SH or SYSTEM intrinsics
AC_FC_CHECK_INTRINSICS([ SH SYSTEM ])

dnl    And record size
AC_FC_RECL_UNIT

AC_FC_OPEN_SPECIFIERS(readonly)

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    This sub-package has no dependencies
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
