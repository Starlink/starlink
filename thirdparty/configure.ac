dnl    Process this file with autoconf to produce a configure script

dnl    This is not a package, so the first two arguments are fake.
AC_INIT(star-thirdparty, 1.0, starlink@jiscmail.ac.uk)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Following sanity-check isn't terribly helpful...
AC_CONFIG_SRCDIR(bootstrap)
STAR_DEFAULTS

AC_PROG_MAKE_SET

dnl    The following list must be kept up-to-date by hand.
dnl    The list is available as the substitution variable @subdirs@.
dnl    (must be kept on one line). 
AC_CONFIG_SUBDIRS([caltech eso heasarc knoxville tclsys ijg rit fsf mit mesa3d kitware apache perlsys sofa erfa])

AC_CONFIG_FILES(Makefile)

AC_OUTPUT
