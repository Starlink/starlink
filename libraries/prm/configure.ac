dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(prm, 1.4-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
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
AC_PROG_LIBTOOL

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

STAR_PLATFORM_SOURCES([num_han.c],
                      [mips _dec_osf _sun_solaris _sun default])

AC_C_BIGENDIAN


dnl    Check for SIND as proxy for the other trig functions which take
dnl    degree arguments, and IZEXT as proxy for related VMS intrinsics.
AC_FC_CHECK_INTRINSICS([izext nint anint sind])



dnl    Determine whether the Fortran compiler supports the VOLATILE keyword
AC_LANG_PUSH([Fortran])
AC_CACHE_CHECK([whether ${FC} supports VOLATILE],
               [ac_cv_fc_has_volatile],
               [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[
      INTEGER X
      VOLATILE X
])],
                                  ac_cv_fc_has_volatile=yes,
                                  ac_cv_fc_has_volatile=no)])
if test $ac_cv_fc_has_volatile = yes; then
    AC_DEFINE([FC_HAVE_KEYWORD_VOLATILE], 1,
              [Define as 1 if the Fortran compiler supports the VOLATILE keyword])
fi


dnl    Determine whether the Fortran compiler supports the 'xxx'X notation
dnl    for integers and reals.  The notation Z"xxx" is the one blessed
dnl    by ISO-1539, but (a) 'xxx'X hasn't caused problems so far, and
dnl    (b) some compilers (for example g77 without the -ftypeless-boz
dnl    option) parse the notation as an integer -- INTEGER(KIND=1) --
dnl    rather than typeless), and (c) in any case ISO-1539 states
dnl    that these boz-constants can appear only in DATA statements.
dnl    If we can't use the 'xxx'X notation, we're therefore better
dnl    off sticking to ordinary decimal notation.
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

dnl    Except some compilers (g95) only support the X'xxx' form, but
dnl    also don't allow integer contants to overflow, so decimal notation
dnl    fails (for the unsigned types). So test for this and apply changes.
dnl    Note it does allow typeless boz for floating points too, so make 
dnl    that part of the test.
AC_CACHE_CHECK([whether ${FC} supports F95 hex notation for all numbers],
   [ac_cv_fc_have_f95_hex_numbers],
   [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [
      INTEGER I
      PARAMETER ( I = X'FF' )
      DOUBLE PRECISION D
      PARAMETER ( D = X'FFEFFFFFFFFFFFFF' )
])],
        ac_cv_fc_have_f95_hex_numbers=yes,
        ac_cv_fc_have_f95_hex_numbers=no)])
if test $ac_cv_fc_have_f95_hex_numbers = yes; then
    AC_DEFINE([FC_HAVE_F95_HEX_NUMBERS], 1,
              [Define as 1 if the Fortran compiler supports X'xxx' numbers])
fi

dnl    Determine whether the Fortran compiler supports the type-kind
dnl    parameter notation (eg, I=1_4), as described in ISO-1539
dnl    section 4.3.1.  Unfortunately, the standard doesn't specify
dnl    what these parameters are, other than that they are as
dnl    returned by the KIND intrinsic function.  We'll test for _8 for
dnl    integers, however, since that's the one we want to use.
AC_CACHE_CHECK([whether ${FC} supports type-kind notation],
   [ac_cv_fc_type_kind],
   [AC_COMPILE_IFELSE(AC_LANG_PROGRAM([], [
      I=1_8
]),
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
STAR_DECLARE_DEPENDENCIES(build, [sae])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun39)

dnl    There is a .msg file in this directory, and this adds Makefile
dnl    rules to generate the xxx_ERR, xxx_err.h and fac_nnn_err files
dnl    which correspond to it.
STAR_MESSGEN(prm_err.msg)

AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_HEADERS([config.h])

dnl    This is the bit that does the actual work
AC_OUTPUT
