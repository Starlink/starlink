dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([polmap],[1.0-1],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([polmap.news])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)
STAR_MONOLITHS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP

dnl    Use libtool for linking since it will try to use the -rpath option
LT_INIT

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build],[psx ndf pgplot],[link])
STAR_DECLARE_DEPENDENCIES([use], [tsp])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun204)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
