dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(shl, 1.0-1, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([shl_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL
AC_FC_LIBRARY_LDFLAGS
AC_FC_MAIN

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Build ADAM task for testing
STAR_MONOLITHS

dnl    This package depends on HLP
STAR_DECLARE_DEPENDENCIES([build], [sae fio cnf hlp mers one psx])
STAR_DECLARE_DEPENDENCIES([link],  [fio cnf hlp par mers one psx])

dnl    Help library translation (for tests)
STAR_CHECK_PROGS(hlib)

dnl    No latex documentation yet. Should it be part of HLP?
dnl STAR_LATEX_DOCUMENTATION(sun124)

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
