dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(findcoords, 1.0-2, starlink@jiscmail.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([findcoords.in])

 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET

dnl    This is a perl program
STAR_CHECK_PROGS(perl)

dnl    Depends on cursa package for use but if we do not specify
dnl    build dependency cursa will not be built first
STAR_DECLARE_DEPENDENCIES([build], [cursa])

dnl    We have a SUN document, so we should probably tell people it's there.
STAR_LATEX_DOCUMENTATION(sun240)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
