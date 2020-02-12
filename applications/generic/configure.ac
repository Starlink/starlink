dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(generic, 1.4, starlink@jiscmail.ac.uk)
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([generic])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS
AC_PROG_CC

dnl    Find required versions of the programs we need for configuration
AC_PROG_LN_S

dnl    Declare the build and use dependencies for this package

STAR_LATEX_DOCUMENTATION(sun7)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
