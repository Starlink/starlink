dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(chr, 2.2-5, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.6)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([chr_upper.f])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_F77
AC_PROG_LIBTOOL

STAR_DECLARE_BUILD_DEPENDENCIES(sae cnf)

STAR_LATEX_DOCUMENTATION(sun40)

STAR_MESSGEN

dnl   Header checks.  There are no such explicit checks at present,
dnl   only the implicit ones.  Also config.h is not at present
dnl   included by anything.  However we might as well include this
dnl   stanza here, to support that in future, but also to keep down
dnl   the length of generated command lines.
AM_CONFIG_HEADER(config.h)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
