dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(par, 2.3, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.6)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([PAR_PAR])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_CC
AC_PROG_F77
AC_PROG_LIBTOOL

STAR_DECLARE_BUILD_DEPENDENCIES(cnf sae mers subpar)
STAR_DECLARE_USE_DEPENDENCIES(chr)

STAR_LATEX_DOCUMENTATION(sun114)

STAR_MESSGEN

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
