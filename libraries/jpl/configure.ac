dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(jpl, 2.0-2, starlink@jiscmail.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([jpl_link])

#   Include defaults for Starlink configurations
STAR_DEFAULTS

#   Checks for programs
AC_PROG_FC
AC_PROG_FPP

#   Need to disable shared libraries as the common blocks are publically
#   available.
AC_DISABLE_SHARED

#   More checks for programs
AC_PROG_LIBTOOL
AC_PROG_LN_S

STAR_CNF_COMPATIBLE_SYMBOLS

dnl  Test what features the OPEN command can support
AC_FC_OPEN_SPECIFIERS(readonly)

dnl  What unit does OPEN's recl specifier assume?
AC_FC_RECL_UNIT

dnl  No dependencies
dnl STAR_DECLARE_DEPENDENCIES([build], [ ])
dnl STAR_DECLARE_DEPENDENCIES([link],  [ ])

dnl  Documentation is uncomplicated
STAR_LATEX_DOCUMENTATION(sun87)

AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES([runtesteph], [chmod 755 runtesteph])

AC_OUTPUT
