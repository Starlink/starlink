dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(hds, 4.3-4, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.6)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([hds_start.c])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_F77
AC_PROG_LIBTOOL

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_BUILD_DEPENDENCIES(sae cnf ems)
STAR_DECLARE_USE_DEPENDENCIES(chr)

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun92)

dnl    There are two .msg files in this directory, so we need to specify
dnl    the names of them explicitly, rather than them being defaulted from
dnl    the package name.
STAR_MESSGEN([cmp_err.msg dat_err.msg])

AM_CONFIG_HEADER(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
