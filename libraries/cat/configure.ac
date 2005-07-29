dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(cat, 8.1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
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
AC_PROG_LIBTOOL

dnl    Other options here, such as
dnl STAR_HAVE_FC_OPEN_READONLY
STAR_CNF_COMPATIBLE_SYMBOLS

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


dnl    Lifted from PRM to determine if the 'xxx'X notation is supported.
dnl    If not use X'xxx' and keep fingers crossed, one has to work
dnl    for CAT1_NUL.
AC_CACHE_CHECK([whether ${FC} supports hex notation for numbers],
   [ac_cv_fc_have_hex_numbers],
   [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [
      INTEGER I
      PARAMETER ( I = 'FF'X )
])],
        ac_cv_fc_have_hex_numbers=yes,
        ac_cv_fc_have_hex_numbers=no)])
if test $ac_cv_fc_have_hex_numbers = yes; then
    AC_DEFINE([FC_HAVE_HEX_NUMBERS], 1,
              [Define as 1 if the Fortran compiler supports 'xxx'X numbers])
fi

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_HEADERS([config.h])

dnl    This is the bit that does the actual work
AC_OUTPUT
