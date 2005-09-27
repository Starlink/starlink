dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(smurf, 0.1.1, t.jenness@jach.hawaii.edu)
PACKAGE_UPCASE='SMURF'
AC_DEFINE( [PACKAGE_UPCASE], "SMURF", [Upper cased package name] )
AC_SUBST(PACKAGE_UPCASE)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([smurf_mon.c])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CHECK_PROGS( ifd2star )
STAR_CHECK_PROGS( hlib )

dnl    External libraries
LIBS="$LIBS -L/usr/lib64"
AC_CHECK_LIB( gslcblas, cblas_dsdot )
AC_CHECK_LIB( gsl, gsl_acosh )

AC_CHECK_HEADERS(math.h)
AC_CHECK_HEADERS(gsl/gsl_math.h)

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [ast sae pcs mers ndf])
STAR_DECLARE_DEPENDENCIES([link], [ndf ast hlp ndg])
dnl    No use dependencies

dnl Documentation
dnl STAR_LATEX_DOCUMENTATION( sun216/ ssn72/ sc10/ sc11/ )

dnl Ask for a config.h so that we can retain control of when to define
dnl parameters. And also to simplify the build line
AC_CONFIG_HEADERS(config.h)

AC_CONFIG_FILES(Makefile smurf.ifd component.xml libsmurf/Makefile)

AC_OUTPUT
