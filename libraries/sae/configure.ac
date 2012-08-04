dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(sae, 1.1, starlink@jiscmail.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([sae_par.h])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

STAR_LATEX_DOCUMENTATION(sun191)

dnl   This message file is here in order to generate the fac_210_err file,
dnl   which is installed in $STARLINK/help
STAR_MESSGEN(sae_err.msg)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
