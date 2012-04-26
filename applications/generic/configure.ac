dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(generic, 1.3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([generic])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
AC_PROG_LN_S

# Check intrinsic zext is available (we rely on iand if it's not)
AC_FC_CHECK_INTRINSICS(zext)

dnl    Check whether we have X'' or ''X notation
AC_FC_HAVE_OLD_TYPELESS_BOZ
AC_FC_HAVE_TYPELESS_BOZ

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, prm)

STAR_LATEX_DOCUMENTATION(sun7)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
