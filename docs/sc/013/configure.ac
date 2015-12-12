dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(sc13, 2-5, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR([sc13.tex])

STAR_DEFAULTS(docs-only)

STAR_LATEX_DOCUMENTATION(sc13)

AC_CONFIG_FILES([Makefile component.xml])

AC_CONFIG_SUBDIRS([examples])

AC_OUTPUT
