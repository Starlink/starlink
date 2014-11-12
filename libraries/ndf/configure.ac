dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([ndf],[1.13-0],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([NDF_PAR])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
LT_INIT

STAR_CHECK_PROGS([fgeneric])

dnl    Need these for determining the time to subsecond accuracy, the
dnl    gettimeofday function is often available but requires special
dnl    handling, so make sure we can compile it, otherwise do without
dnl    (Solaris requires that _POSIX_C_SOURCE is not defined, amongst others,
dnl    Tru64 that _XOPEN_SOURCE=500, Mac OS X doesn't seem to define
dnl    suseconds_t...)
AC_CHECK_HEADERS(sys/time.h)
AC_CACHE_CHECK([gettimeofday compiles],
   [ac_cv_gettimeofday],
   [AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#define _POSIX_SOURCE 1
#include <stdio.h>
#include <sys/time.h>
void check() {
   struct timeval tv;
   suseconds_t usec = 0;
   int result;
   result = gettimeofday( &tv, NULL );
}])],
        ac_cv_gettimeofday=yes,
        ac_cv_gettimeofday=no)])
if test $ac_cv_gettimeofday = yes; then
    AC_DEFINE([HAVE_GETTIMEOFDAY], 1,
       [Define to 1 when gettimeofday can be compiled with <sys/time.h>])
fi

dnl   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS

dnl   We need this for the tests
AC_FC_LIBRARY_LDFLAGS
AC_FC_MAIN

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae prm hds chr ems ary ast psx generic])
STAR_DECLARE_DEPENDENCIES([link],  [psx ary hds ems chr prm ast])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(ssn20 sun33)

STAR_MESSGEN(ndf_err.msg)


dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
