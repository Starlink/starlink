dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
  
dnl    Initialisation: package name and version number
AC_INIT(idi, 1.4-5, starlink@jiscmail.ac.uk)
  
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
  
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([IDI_PAR])


dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_HEADER_STDC
AC_PATH_XTRA
AC_PROG_LIBTOOL
AC_C_BIGENDIAN

dnl    Having no X11 is fatal.
if test "$no_x" = "yes"; then
   AC_MSG_ERROR([No X11 development system found. Cannot continue])
fi

AC_CHECK_HEADERS([X11/decwmhints.h])
AC_CHECK_HEADERS([unixlib.h])

AC_CHECK_LIB([m],[sin])
AC_CHECK_FUNCS([nint rint])
AC_CHECK_DECLS([rint],,,[#include <math.h>])

AC_FC_LIBRARY_LDFLAGS

dnl    make check requires starlink monolith
STAR_MONOLITHS

STAR_CNF_COMPATIBLE_SYMBOLS
 
dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae cnf ems gns gwm par starx])
STAR_DECLARE_DEPENDENCIES([link],  [cnf ems gwm gns starx])
 
dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun65)

STAR_MESSGEN(idi_err.msg)
 
dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)
 
dnl    This is the bit that does the actual work
AC_OUTPUT
