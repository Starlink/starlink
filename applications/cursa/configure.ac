dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(cursa, 6.4-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([cursa.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CHECK_PROGS([perl])
STAR_CHECK_PROGS([expectk])
STAR_CHECK_PROGS([awish])
STAR_CHECK_PROGS([atclsh])

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [sae cat fio par pda ndf pgp])

STAR_LATEX_DOCUMENTATION([sun190 ssn76])

AC_CONFIG_SUBDIRS([libcap])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
