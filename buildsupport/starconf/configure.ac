dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(starconf, 1.0, ussc@star.rl.ac.uk)
AM_INIT_AUTOMAKE

AC_CONFIG_SRCDIR([starlink.m4.in])

dnl The default installation location is /star.  Note that this
dnl specifies both the default location of this component AND the
dnl default prefix for all components which use `starconf' and the
dnl acinclude.m4 which it generates.
AC_PREFIX_DEFAULT(/star)

dnl Find a working install program
AC_PROG_INSTALL

dnl Note that the starconf.in and starlink.m4.in files are _not_ to be
dnl substituted by this command -- since they involve they
dnl installation directory variables, they should have them replaced
dnl at `make' time, not configure time.  See Makefile.am for details.
AC_CONFIG_FILES(Makefile)

AC_OUTPUT
