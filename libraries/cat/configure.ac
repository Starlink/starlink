dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([cat],[8.4],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([CAT_PAR])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
LT_INIT

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS([fgeneric])

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [ast sae par prm])
STAR_DECLARE_DEPENDENCIES(link,  [cfitsio par mers sla chr ast])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun181)

dnl    There is a .msg file in this directory, and this adds Makefile
dnl    rules to generate the xxx_ERR, xxx_err.h and fac_nnn_err files
dnl    which correspond to it.
STAR_MESSGEN(cat_err.msg)

dnl    Determine if the 'xxx'X notation is supported.
dnl    If not use X'xxx' and keep fingers crossed, one has to work
dnl    for CAT1_NUL.
AC_FC_HAVE_BOZ
AC_FC_HAVE_TYPELESS_BOZ
AC_FC_HAVE_OLD_TYPELESS_BOZ

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_HEADERS([config.h])

dnl    This is the bit that does the actual work
AC_OUTPUT
