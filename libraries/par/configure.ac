dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT([par],[2.4-0],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl   Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl   Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([PAR_PAR])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS
STAR_MONOLITHS

AC_PROG_CC
AC_PROG_FC
LT_INIT

STAR_CHECK_PROGS([fgeneric])

STAR_CNF_COMPATIBLE_SYMBOLS

STAR_DECLARE_DEPENDENCIES([build], [cnf generic mers pcs sae])
STAR_DECLARE_DEPENDENCIES([link], [chr mers pcs])

STAR_LATEX_DOCUMENTATION(sun114)

STAR_MESSGEN(par_err.msg)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
