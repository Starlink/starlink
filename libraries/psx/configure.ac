dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(psx, 0.4, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([psx_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_CHECK_FUNCS([getpwuid geteuid getlogin cuserid])
AC_CHECK_DECLS([cuserid])
AC_CHECK_FUNCS([setenv])
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun121)

STAR_MESSGEN(psx_err.msg)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
