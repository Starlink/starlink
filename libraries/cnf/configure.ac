dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(cnf, 4.1-1, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([f77.h])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP(substitute)
AC_PROG_LIBTOOL
AC_PROG_LN_S
AC_FC_MAIN

dnl   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_CNF_COMPATIBLE_SYMBOLS

dnl   We need this for the tests
AC_FC_LIBRARY_LDFLAGS

dnl   Check if BSD bcopy function is available, if not should use memmove.
AC_CHECK_FUNCS(memmove bcopy)

dnl   Need to know the size of a pointer for CNF_PVAL
dnl   (see CNF_PAR.F and Makefile.am)
AC_CHECK_SIZEOF(void*)
AC_CHECK_SIZEOF(uint32_t)

dnl   This package has no dependencies

STAR_LATEX_DOCUMENTATION(sun209)

dnl   Header checks.  
AM_CONFIG_HEADER(config.h)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
