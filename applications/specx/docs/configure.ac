dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(specx-docs, 6.8-5, t.jenness@jach.hawaii.edu)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([configure.ac])

dnl    Include defaults for Starlink configurations
dnl    SPECX is currently per-package-dir but that is not really
dnl    necessary since SYS_SPECX just needs to be in one place.
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET

dnl    Need to traverse sub directories
AC_CONFIG_SUBDIRS([help sun17 sc8 ])

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
