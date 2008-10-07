dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(gsd, 1.0-2, t.jenness@jach.hawaii.edu)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([GSD_PAR])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_HEADER_STDC
AC_C_BIGENDIAN
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Make sure the variable types are okay for GSD
AC_CHECK_TYPES([int8_t, int16_t, int32_t, int64_t])
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short int)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [prm cnf])
STAR_DECLARE_DEPENDENCIES(link,  [prm cnf])

dnl    Documentation is in the parent directory
STAR_LATEX_DOCUMENTATION(sun229)

dnl    There is no .msg file in this directory. 

dnl   Header checks. 
AM_CONFIG_HEADER(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
