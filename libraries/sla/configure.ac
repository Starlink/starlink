dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(sla, 2.4-11, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.6)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sla_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_F77
AC_PROG_LIBTOOL

dnl    Declare the build and use dependencies for this package
dnl    There are neither build nor use dependencies

STAR_LATEX_DOCUMENTATION(sun67)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
