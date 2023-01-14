dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT([startcl],[1.5-7],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR([tclAdam.c])
STAR_DEFAULTS

AC_PROG_CC
AC_PROG_FC
LT_INIT
AC_HEADER_STDC
AC_CHECK_HEADERS(malloc.h)

dnl    Uses C compiler to link against Fortran libraries, so we need
dnl    this.
AC_FC_LIBRARY_LDFLAGS

AC_FC_MAIN

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    round() is in libm
AC_CHECK_LIB([m],[round])
AC_CHECK_FUNCS(round)

dnl    Just check we have the JPEG headers for sanity. This is a build
dnl    dependency, so should be guaranteed.
AC_CHECK_HEADERS([jpeglib.h])

STAR_DECLARE_DEPENDENCIES(build, [ems gwm jpeg mers nbs pcs sae starx tcl tk])
STAR_DECLARE_DEPENDENCIES(configure, [jpeg tcl tk])

STAR_LATEX_DOCUMENTATION([sun186 sun219])

dnl    Locate the tclsh and wish binaries and associated libraries
STAR_PATH_TCLTK(8.6,[tk])

dnl    How to link to X.
AC_PATH_XTRA

# Define STARTCL_LIBRARY as a macro for baking into local binaries.
STARTCL_LIBRARY=\"${prefix}/lib/startcl\"
AC_DEFINE_UNQUOTED(STARTCL_LIBRARY, $STARTCL_LIBRARY, [Compile time library directory])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
