dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 26007 $)

dnl    Initialisation: package name and version number
AC_INIT(gaiavo, 1.0-0, gaia@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([VOTable.C])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_CXX

dnl    We require a version of Xerces-c to be installed.
dnl    Difficult to check for library linkage, so just use header check.
AC_CHECK_HEADER([xercesc/util/XercesVersion.hpp], [], 
    AC_MSG_ERROR([Couldn't find xerces-c headers. Need xerces-c.]))

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile])

dnl    This is the bit that does the actual work
AC_OUTPUT
        
