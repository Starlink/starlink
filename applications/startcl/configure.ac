dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(startcl, 1.5-5, ussc@star.rl.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR([tclAdam.c])
STAR_DEFAULTS

AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL
AC_STDC_HEADERS
AC_CHECK_HEADERS(malloc.h)

STAR_CNF_COMPATIBLE_SYMBOLS

AC_CHECK_LIB(jpeg,jpeg_start_compress)

STAR_DECLARE_DEPENDENCIES(build, [starx adam ams dtask ems gwm messys mers tcl tk nbs sae])

STAR_LATEX_DOCUMENTATION(sun186)
STAR_LATEX_DOCUMENTATION(sun219)

dnl    Locate the tclsh and wish binaries and associated libraries
STAR_PATH_TCLTK(8.2,[tk])

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
