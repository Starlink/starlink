dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(prm, 1.3-1, ussc@star.rl.ac.uk)

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

dnl    Other options here, such as
dnl STAR_HAVE_F77_OPEN_READONLY
STAR_CNF_COMPATIBLE_SYMBOLS

AC_CHECK_HEADERS([fenv.h limits.h float.h stdint.h time.h])
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



dnl  Determine the intrinsics available to the Fortran compiler.
dnl
dnl  This macro should go in autoconf/lib/autoconf/fortran.m4
dnl
dnl  I think it should be safe enough to give a real argument to each
dnl  intrinsic below, and assign it to a real result -- that's more likely
dnl  to cause run-time than compilation or link errors, which are what we
dnl  care about here.
dnl
AC_DEFUN([AC_FC_HAS_INTRINSICS],
         [AC_REQUIRE([AC_PROG_FC])
          AC_LANG_PUSH([Fortran])
          AC_FOREACH([IntrinsicName], m4_translit([$1], [a-z], [A-Z]),
                     [AC_CACHE_CHECK([whether ${FC} supports intrinsic ]IntrinsicName,
                                     [ac_cv_fc_has_]IntrinsicName,
                                     [AC_LINK_IFELSE([AC_LANG_PROGRAM([],[
      REAL X
      X = IntrinsicName[(1.)]
])],
                                          ac_cv_fc_has_[]IntrinsicName=yes,
                                          ac_cv_fc_has_[]IntrinsicName=no)])
                      if test $ac_cv_fc_has_[]IntrinsicName = yes; then
                          AC_DEFINE([HAVE_INTRINSIC_]IntrinsicName, 1,
                                    [Define to 1 if the Fortran compiler supports intrinsic ]IntrinsicName)
                      fi
                     ])
         AC_LANG_POP([Fortran])
])# AC_FC_HAS_INTRINSICS


dnl    Check for SIND as proxy for the other trig functions which take
dnl    degree arguments, and IZEXT as proxy for related VMS intrinsics.
AC_FC_HAS_INTRINSICS([izext nint anint sind])



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
dnl    for integers and reals.
AC_CACHE_CHECK([whether ${FC} supports hex notation for numbers],
               [ac_cv_fc_has_hex_numbers],
               [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [
      INTEGER I
      PARAMETER ( I = 'FF'X )
])],
                                  ac_cv_fc_has_hex_numbers=yes,
                                  ac_cv_fc_has_hex_numbers=no)])
if test $ac_cv_fc_has_hex_numbers = yes; then
    AC_DEFINE([FC_HAVE_HEX_NUMBERS], 1,
              [Define as 1 if the Fortran compiler supports 'xxx'X numbers])
fi



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
