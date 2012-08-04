dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
  
dnl    Initialisation: package name and version number
AC_INIT(gwm, 1.5-1, starlink@jiscmail.ac.uk)
  
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
  
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([gwm.h])
 
  
dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_HEADER_STDC
AC_PATH_XTRA
AC_PROG_LIBTOOL

dnl    Having no X11 is fatal.
if test "$no_x" = "yes"; then
   AC_MSG_ERROR([No X11 development system found. Cannot continue])
fi

STAR_CNF_COMPATIBLE_SYMBOLS
 
dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae cnf ems])
STAR_DECLARE_DEPENDENCIES([link],  [starx ems cnf])
 
dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun130)

STAR_MESSGEN(gwm_err.msg)
 
 
dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)
 
dnl    This is the bit that does the actual work
AC_OUTPUT
