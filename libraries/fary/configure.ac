dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([fary],[1.4],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([ARY_PAR])


dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
LT_INIT

STAR_CHECK_PROGS([fgeneric])

dnl    If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae prm hds chr ems generic])
STAR_DECLARE_DEPENDENCIES([link],  [hds ems chr prm])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun11)

STAR_MESSGEN(ary_err.msg)


dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
