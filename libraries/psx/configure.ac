dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(psx, 0.4, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.6)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([psx_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_LIBTOOL

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_BUILD_DEPENDENCIES(sae)
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun121)

STAR_MESSGEN

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
