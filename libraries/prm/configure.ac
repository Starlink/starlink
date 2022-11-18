dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([prm],[1.6-1],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([prm_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
LT_INIT

STAR_CHECK_PROGS([fgeneric])

#   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_CNF_COMPATIBLE_SYMBOLS

AC_CHECK_HEADERS([fenv.h limits.h float.h time.h assert.h inttypes.h stdint.h])
AC_CHECK_FUNCS([nextafter nextafterf])
AC_CHECK_TYPES([int32_t, int64_t])
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(short int)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long int)
AC_CHECK_SIZEOF(long long int)

AC_C_BIGENDIAN


dnl    Check for SIND as proxy for the other trig functions which take
dnl    degree arguments, and IZEXT as proxy for related VMS intrinsics.
AC_FC_CHECK_INTRINSICS([izext nint anint sind])

dnl    Determine whether the Fortran compiler supports the VOLATILE keyword
AC_FC_HAVE_VOLATILE

dnl    Determine whether the Fortran compiler supports the 'xxx'X notation
dnl    or the X'xxx' notation.
dnl
dnl    This is used for assigning integers and reals using a typeless
dnl    transformation of the given bit patterns. Note the other BOZ
dnl    descriptors are just usually integers in disguise, so like plain
dnl    numbers may also require compiler support to work with PRM, see
dnl    STAR_PRM_COMPATIBLE_SYMBOLS. For this reason we only use plain
dnl    numbers or the 'X' descriptor.
AC_FC_HAVE_OLD_TYPELESS_BOZ()
AC_FC_HAVE_TYPELESS_BOZ()
STAR_PRM_COMPATIBLE_SYMBOLS

dnl    Determine whether the Fortran compiler supports the type-kind
dnl    parameter notation (eg, I=1_4), as described in ISO-1539
dnl    section 4.3.1.  Unfortunately, the standard doesn't specify
dnl    what these parameters are, other than that they are as
dnl    returned by the KIND intrinsic function.  We'll test for _8 for
dnl    integers, however, since that's the one we want to use.
dnl    PWD: note in passing, this test always fails as it does not use
dnl    Fortran, might be just as well.
AC_CACHE_CHECK([whether ${FC} supports type-kind notation],
   [ac_cv_fc_type_kind],
   [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [
      I=1_8
])],
        ac_cv_fc_type_kind=yes,
        ac_cv_fc_type_kind=no)])
if test $ac_cv_fc_type_kind = yes; then
    AC_DEFINE([FC_HAVE_TYPE_KIND], 1,
       [Define to 1 if the Fortran compiler supports type-kind notation])
fi


dnl    So they can be noted in the generated parameter files, pass the
dnl    $build and FC variables in to config.h
AC_DEFINE_UNQUOTED([INFO_BUILD], "$build",
    "The config.guess value for the build system")
AC_DEFINE_UNQUOTED([INFO_FC],    "$FC",
    "The Fortran compiler whose characteristics were tested")


dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [sae generic])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun39)

dnl    There is a .msg file in this directory, and this adds Makefile
dnl    rules to generate the xxx_ERR, xxx_err.h and fac_nnn_err files
dnl    which correspond to it.
STAR_MESSGEN(prm_err.msg)

AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES([make_h], [chmod +x make_h])
AC_CONFIG_HEADERS([config.h])

dnl    This is the bit that does the actual work
AC_OUTPUT
