dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(tsp, 2.3-4, starlink@jiscmail.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([tsp.news])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)
STAR_MONOLITHS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
STAR_CHECK_PROGS([ifd2star])

dnl    Use libtool for linking since it will try to use the -rpath option
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Private error file
STAR_MESSGEN(user_err.msg)

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build],[sla figaro ndf ifd snx ncar],[link])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun66)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml tsp.ifd)

dnl    This is the bit that does the actual work
AC_OUTPUT
