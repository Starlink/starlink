dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(hdspar, 1.0-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.6)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([hdspar_link_adam])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_F77
AC_PROG_LIBTOOL

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_BUILD_DEPENDENCIES(sae)
STAR_DECLARE_USE_DEPENDENCIES(subpar)

dnl    Documented in SUN/224, but that's in the parent directory
dnl STAR_LATEX_DOCUMENTATION(sun224)

AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
