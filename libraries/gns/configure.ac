dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(gns, 1.1-4, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([GNS_PAR])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

dnl    Look for unit of RECL
AC_FC_RECL_UNIT


STAR_CNF_COMPATIBLE_SYMBOLS

dnl    GNS does sed replacement on RECL as well. Need to look at that

AC_FC_OPEN_SPECIFIERS(readonly)
AC_FC_OPEN_SPECIFIERS(action='read')

AC_CONFIG_HEADERS([config.h])


dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae psx ems chr gks gwm])
STAR_DECLARE_DEPENDENCIES([link],  [psx ems chr gks gwm])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun57)

STAR_MESSGEN(gns_err.msg)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
