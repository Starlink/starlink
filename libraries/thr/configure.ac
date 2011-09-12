dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(thr, 1.0, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([thr.c])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_LIBTOOL

#   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

dnl  Look for pthreads library
AC_CHECK_HEADERS(pthread.h)
AC_CHECK_LIB([pthread],[pthread_mutex_lock])


dnl    Look for standard headers rather than assuming availability
dnl    by operating system
AC_HEADER_STDC

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [ast ems])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
dnl    STAR_LATEX_DOCUMENTATION(sun)

AM_CONFIG_HEADER(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
