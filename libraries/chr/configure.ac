dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT([chr],[3.0],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl   Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([chr_upper.f])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_FC
LT_INIT

dnl   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_CNF_COMPATIBLE_SYMBOLS

dnl We need to be compatible with PRM for the INTEGER*8 tests
STAR_PRM_COMPATIBLE_SYMBOLS

STAR_DECLARE_DEPENDENCIES([build], [sae cnf])

STAR_LATEX_DOCUMENTATION(sun40)

dnl   We also require an _ext file for the include file
STAR_MESSGEN(chr_err.msg)

dnl   Header checks.  There are no such explicit checks at present,
dnl   only the implicit ones.  Also config.h is not at present
dnl   included by anything.  However we might as well include this
dnl   stanza here, to support that in future, but also to keep down
dnl   the length of generated command lines.
AM_CONFIG_HEADER(config.h)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
