dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(cnf, 4.0-2, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([cnf.h])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL
AC_PROG_LN_S

STAR_CNF_COMPATIBLE_SYMBOLS

dnl   This package has no dependencies

STAR_LATEX_DOCUMENTATION(sun209)

dnl   Header checks.  There are no such explicit checks at present,
dnl   only the implicit ones.  Also config.h is not at present
dnl   included by anything.  However we might as well include this
dnl   stanza here, to support that in future, but also to keep down
dnl   the length of generated command lines.
AM_CONFIG_HEADER(config.h)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
