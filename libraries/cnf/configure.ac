dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(cnf, 5.0-0, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([f77.h.in])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP(substitute)
AC_PROG_LIBTOOL
AC_PROG_LN_S
AC_FC_MAIN

dnl   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_INITIALISE_FORTRAN
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Check for %LOC, if not present assume we have LOC.
AC_FC_HAVE_PERCENTLOC

dnl   We need this for the tests
AC_FC_LIBRARY_LDFLAGS

dnl   Check if BSD bcopy function is available, if not should use memmove.
AC_CHECK_FUNCS(memmove bcopy)

dnl   Need to know the size of a pointer for CNF_PVAL
dnl   (see CNF_PAR.F and Makefile.am)
AC_CHECK_SIZEOF(void*)
AC_CHECK_SIZEOF(uint32_t)

dnl   Declare dependencies
STAR_DECLARE_DEPENDENCIES([build], [starmem])
STAR_DECLARE_DEPENDENCIES([link],  [starmem])

dnl   Check the return type of REAL Fortran functions. Under g77
dnl   in -ff2c mode (the default), these return a double. Only
dnl   seems to have an effect under 64-bit. Defines REAL_FUNCTION_TYPE.
dnl   to double or float.
STAR_CNF_F2C_COMPATIBLE

dnl   Determine the symbol used for blank common. Defines BLANK_COMMON_SYMBOL.
STAR_CNF_BLANK_COMMON

dnl   Type used for trailing lengths of character strings, int or long.
STAR_CNF_TRAIL_TYPE

dnl   Need to know size of these trailing character lengths for CNF_CVAL.
if test "$star_cv_cnf_trail_type" = "int"; then
   AC_CHECK_SIZEOF([int])
   AC_DEFINE_UNQUOTED(SIZEOF_TRAILARG,$ac_cv_sizeof_int,
                      [Size of TRAIL argument])
else
   AC_CHECK_SIZEOF([long])
   AC_DEFINE_UNQUOTED(SIZEOF_TRAILARG,$ac_cv_sizeof_long,
                      [Size of TRAIL argument])
fi

dnl   This package has no dependencies

STAR_LATEX_DOCUMENTATION(sun209)

dnl   Header checks.
AM_CONFIG_HEADER(config.h)

AC_CONFIG_FILES(Makefile component.xml f77.h)

AC_OUTPUT
