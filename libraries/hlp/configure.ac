dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(hlp, 3.3-2, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.6)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([hlp_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_F77
AC_PROG_FPP
AC_PROG_LIBTOOL

STAR_FPP_SOURCES(fopr)
STAR_HAVE_F77_OPEN_READONLY

AC_CONFIG_HEADERS([config.h])

dnl    Declare the build and use dependencies for this package
dnl    No dependencies at all!
dnl    STAR_DECLARE_BUILD_DEPENDENCIES()
dnl    STAR_DECLARE_USE_DEPENDENCIES()

STAR_LATEX_DOCUMENTATION(sun124)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
