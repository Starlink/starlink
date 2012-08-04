dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(hlp, 4.0-3, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([hlp_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
dnl    Look for a fortran compiler for the test program
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

AC_CONFIG_HEADERS([config.h])

dnl    This package needs CNF for the fortran interface
STAR_DECLARE_DEPENDENCIES([build], [cnf])
STAR_DECLARE_DEPENDENCIES([link],  [cnf])
STAR_DECLARE_DEPENDENCIES([test],  [cnf])


STAR_LATEX_DOCUMENTATION(sun124)

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
