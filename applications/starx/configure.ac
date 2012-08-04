dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(starx, 1.2-1, starlink@jiscmail.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([starx_test.c])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_HEADER_STDC
AC_PATH_XTRA
AC_PROG_LIBTOOL
 
STAR_CNF_COMPATIBLE_SYMBOLS
 
dnl    Documentation is in the parent directory
STAR_LATEX_DOCUMENTATION(ssn21)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES(starX_link, [chmod +x starX_link])

dnl    This is the bit that does the actual work
AC_OUTPUT
