dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(chr, 2.2-5, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([chr_upper.f])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

dnl   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_CNF_COMPATIBLE_SYMBOLS

STAR_DECLARE_DEPENDENCIES([build], [sae cnf])

STAR_LATEX_DOCUMENTATION(sun40)

dnl   We also require an _ext file for the include file
STAR_MESSGEN(chr_err.msg)

dnl   Check how backslashes are handled.
AC_FC_LITERAL_BACKSLASH

dnl   Header checks.  
AM_CONFIG_HEADER(config.h)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
