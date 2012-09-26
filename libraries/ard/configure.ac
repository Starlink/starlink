dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(ard, 2.3, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([ard_work.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC([],77)
AC_PROG_FPP
AC_PROG_LIBTOOL

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS

dnl   We need this for the tests
AC_FC_LIBRARY_LDFLAGS

dnl    Check for %LOC, if not present assume we have LOC.
AC_FC_HAVE_PERCENTLOC()

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [ast cnf grp pcs sae])
STAR_DECLARE_DEPENDENCIES(link, [ast chr mers psx])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun183)

dnl    There is a .msg file in this directory, and this adds Makefile
dnl    rules to generate the xxx_ERR, xxx_err.h and fac_nnn_err files
dnl    which correspond to it.
STAR_MESSGEN(ard_err.msg)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
