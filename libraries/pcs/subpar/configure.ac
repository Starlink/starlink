dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(subpar, 2.1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sup_accpt.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

STAR_HAVE_F77_OPEN_READONLY
STAR_F77_C_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_BUILD_DEPENDENCIES(sae messys chr adam lex ems hds)
dnl    FAMS_* functions are in the AMS package
STAR_DECLARE_USE_DEPENDENCIES(chr dat ems ams hds hlp icl lex parsecon psx)

dnl    There is a .msg file in this directory, and this adds Makefile
dnl    rules to generate the xxx_ERR, xxx_err.h and fac_nnn_err files
dnl    which correspond to it.
STAR_MESSGEN(subpar_err.msg)

AC_CONFIG_HEADERS(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
