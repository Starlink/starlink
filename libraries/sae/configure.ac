dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(sae, 1.0, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.6)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([sae_par.h])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

STAR_LATEX_DOCUMENTATION(sun191)

dnl   There is no sae_err.msg at present, only the hand-edited (?)
dnl   generated file fac_210_err.  When we get a proper
dnl   source file for these, uncomment the following.  It's possible
dnl   that there is no meaningful source file for these, in which case
dnl   that should be explained here.
dnl STAR_FACILITY_CODE(210)
STAR_MESSGEN

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
