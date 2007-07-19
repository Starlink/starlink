dnl Process this file with autoconf to produce a configure script.

AC_INIT([gsl],[1.9])
AC_CONFIG_SRCDIR(gsl_math.h)

AM_INIT_AUTOMAKE([gnu no-dependencies])
AC_CONFIG_HEADERS([config.h])
AM_MAINTAINER_MODE

dnl Library versioning (current:revision:age)
dnl See the libtool manual for an explanation of the numbers
dnl
dnl gsl-1.0    libgsl 0:0:0  libgslcblas 0:0:0
dnl gsl-1.1    libgsl 1:0:1  libgslcblas 0:0:0
dnl gsl-1.1.1  libgsl 2:0:2  libgslcblas 0:0:0
dnl gsl-1.2    libgsl 3:0:3  libgslcblas 0:0:0
dnl gsl-1.3    libgsl 4:0:4  libgslcblas 0:0:0
dnl gsl-1.4    libgsl 5:0:5  libgslcblas 0:0:0
dnl gsl-1.5    libgsl 6:0:6  libgslcblas 0:0:0
dnl gsl-1.6    libgsl 7:0:7  libgslcblas 0:0:0
dnl gsl-1.7    libgsl 8:0:8  libgslcblas 0:0:0
dnl gsl-1.8    libgsl 9:0:9  libgslcblas 0:0:0
dnl gsl-1.9    libgsl 10:0:10  libgslcblas 0:0:0

GSL_LT_VERSION="10:0:10"
AC_SUBST(GSL_LT_VERSION)
GSL_LT_CBLAS_VERSION="0:0:0"
AC_SUBST(GSL_LT_CBLAS_VERSION)

case "$VERSION" in
    *+)
        AC_DEFINE(RELEASED,0)
        ;;
    *)
        AC_DEFINE(RELEASED,1)
        ;;
esac
AC_SUBST(RELEASED)      

dnl things required by automake
dnl AC_ARG_PROGRAM
AC_PROG_MAKE_SET

dnl Check for which system.
AC_CANONICAL_HOST

dnl Checks for programs.
AC_LANG(C)
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S

dnl Disable unnecessary libtool tests for c++,fortran,java
define([AC_LIBTOOL_LANG_CXX_CONFIG], [:])dnl
define([AC_LIBTOOL_LANG_F77_CONFIG], [:])dnl
define([AC_LIBTOOL_LANG_GCJ_CONFIG], [:])dnl
AC_PROG_LIBTOOL

dnl Check compiler features
AC_TYPE_SIZE_T
dnl AC_C_CONST
AC_C_VOLATILE
AC_C_INLINE

GSL_CFLAGS="-I$includedir"
GSL_LIBS="-L$libdir -lgsl"

AC_SUBST(GSL_CFLAGS)
AC_SUBST(GSL_LIBS)

dnl Check for "extern inline", using a modified version of the test
dnl for AC_C_INLINE from acspecific.mt
dnl
AC_CACHE_CHECK([for extern inline], ac_cv_c_extern_inline,
[ac_cv_c_extern_inline=no
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[extern $ac_cv_c_inline double foo(double x);
extern $ac_cv_c_inline double foo(double x) { return x + 1.0 ; } ;
double foo (double x) { return x + 1.0 ; };]], [[  foo(1.0)  ]])],[ac_cv_c_extern_inline="yes"],[])
])

if test "$ac_cv_c_extern_inline" != no ; then
  AC_DEFINE(HAVE_INLINE,1)
  AC_SUBST(HAVE_INLINE)
fi

dnl Checks for header files.
AC_CHECK_HEADERS(ieeefp.h)

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

dnl AC_FUNC_ALLOCA
AC_FUNC_VPRINTF

dnl strcasecmp, strerror, xmalloc, xrealloc, probably others should be added.
dnl removed strerror from this list, it's hardcoded in the err/ directory
dnl Any functions which appear in this list of functions should be provided
dnl in the utils/ directory
dnl xmalloc is not used, removed (bjg)
AC_REPLACE_FUNCS(memcpy memmove strdup strtol strtoul)

AC_CACHE_CHECK(for EXIT_SUCCESS and EXIT_FAILURE,
ac_cv_decl_exit_success_and_failure,
AC_EGREP_CPP(yes,
[
#include <stdlib.h>
#ifdef EXIT_SUCCESS
yes
#endif
], 
ac_cv_decl_exit_success_and_failure=yes,
ac_cv_decl_exit_success_and_failure=no)
)

if test "$ac_cv_decl_exit_success_and_failure" = yes ; then
  AC_DEFINE(HAVE_EXIT_SUCCESS_AND_FAILURE)
fi ;

dnl Use alternate libm if specified by user

if test "x$LIBS" = "x" ; then
  AC_CHECK_LIB(m, cos)
fi

dnl Remember to put a definition in acconfig.h for each of these
AC_CHECK_DECLS(feenableexcept,,,[#define _GNU_SOURCE 1
#include <fenv.h>]) 
AC_CHECK_DECLS(fesettrapenable,,,[#define _GNU_SOURCE 1
#include <fenv.h>]) 
AC_CHECK_DECLS(hypot,,,[#include <math.h>]) 
AC_CHECK_DECLS(expm1,,,[#include <math.h>])
AC_CHECK_DECLS(acosh,,,[#include <math.h>])
AC_CHECK_DECLS(asinh,,,[#include <math.h>])
AC_CHECK_DECLS(atanh,,,[#include <math.h>])
AC_CHECK_DECLS(ldexp,,,[#include <math.h>])
AC_CHECK_DECLS(frexp,,,[#include <math.h>])
AC_CHECK_DECLS(isinf,,,[#include <math.h>])
AC_CHECK_DECLS(finite,,,[#include <math.h>])
AC_CHECK_DECLS(isfinite,,,[#include <math.h>])
AC_CHECK_DECLS(isnan,,,[#include <math.h>])

dnl OpenBSD has a broken implementation of log1p.
case "$host" in
    *-*-*openbsd*)
       AC_MSG_RESULT([avoiding OpenBSD system log1p - using gsl version])
       ;;
    *)
        AC_CHECK_DECLS(log1p,,,[#include <math.h>])
       ;;
esac

AC_CACHE_CHECK([for long double stdio], ac_cv_func_printf_longdouble,
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <stdio.h>
int main (void) 
{ 
const char * s = "5678"; long double x = 1.234 ; 
fprintf(stderr,"%Lg\n",x) ; 
sscanf(s, "%Lg", &x);
if (x == 5678) {exit (0);} else {exit(1); };
}]])],[ac_cv_func_printf_longdouble="yes"],[ac_cv_func_printf_longdouble="no"],[ac_cv_func_printf_longdouble="no"])])

if test "$ac_cv_func_printf_longdouble" != no; then
  AC_DEFINE(HAVE_PRINTF_LONGDOUBLE,1)
  AC_SUBST(HAVE_PRINTF_LONGDOUBLE)
fi

AC_CACHE_CHECK([for extended floating point registers],ac_cv_c_extended_fp,
[case "$host" in
    *sparc*-*-*)
        ac_cv_c_extended_fp=no
        ;;     
    *powerpc*-*-*)
        ac_cv_c_extended_fp=no
        ;;      
    *hppa*-*-*)
        ac_cv_c_extended_fp=no
        ;;      
    *alpha*-*-*)
        ac_cv_c_extended_fp=no
        ;;      
    *68k*-*-*)
        ac_cv_c_extended_fp=yes
        ;;      
    *86-*-*)
        ac_cv_c_extended_fp=yes
        ;;      
    x86_64-*-*)
        ac_cv_c_extended_fp=yes
        ;;      
    *) 
        ac_cv_c_extended_fp=unknown
        ;;
esac
])

if test $ac_cv_c_extended_fp != "no" ; then
    AC_DEFINE(HAVE_EXTENDED_PRECISION_REGISTERS,1)
    AC_SUBST(HAVE_EXTENDED_PRECISION_REGISTERS)
fi

AC_CACHE_CHECK([for IEEE arithmetic interface type], ac_cv_c_ieee_interface,
[case "$host" in
    sparc-*-linux*) 
        ac_cv_c_ieee_interface=gnusparc
        ;;
    m68k-*-linux*) 
        ac_cv_c_ieee_interface=gnum68k
        ;;
    powerpc-*-linux*) 
        ac_cv_c_ieee_interface=gnuppc
        ;;
    *86-*-gnu | *86_64-*-gnu | *86-*-linux* | *86_64-*-linux*) 
        ac_cv_c_ieee_interface=gnux86
        ;;
    *-*-sunos4*) 
        ac_cv_c_ieee_interface=sunos4
        ;;
    *-*-solaris*) 
        ac_cv_c_ieee_interface=solaris
        ;;
    *-*-hpux11*) 
        ac_cv_c_ieee_interface=hpux11
        ;;
    *-*-hpux*) 
        ac_cv_c_ieee_interface=hpux
        ;;
    *-*-osf*) 
        ac_cv_c_ieee_interface=tru64
        ;;
    *-*-aix*) 
        ac_cv_c_ieee_interface=aix
        ;;
    *-*-irix*) 
        ac_cv_c_ieee_interface=irix
        ;;
    powerpc-*-*darwin*) 
        ac_cv_c_ieee_interface=darwin
        ;;
    *86-*-*darwin*) 
        ac_cv_c_ieee_interface=darwin86
        ;;
    *-*-*netbsd*) 
        ac_cv_c_ieee_interface=netbsd
        ;;
    *-*-*openbsd*)  
        ac_cv_c_ieee_interface=openbsd
        ;;
    *-*-*bsd*) 
        ac_cv_c_ieee_interface=freebsd
        ;;
    *-*-os2*)
        ac_cv_c_ieee_interface=os2emx
        ;;
    *)
        ac_cv_c_ieee_interface=unknown
        ;;
esac
])

if test "$ac_cv_c_ieee_interface" = "gnux86" ; then
    AC_CACHE_CHECK([for FPU_SETCW], ac_cv_c_fpu_setcw,
    [ac_cv_c_fpu_setcw=no
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <fpu_control.h>
#ifndef _FPU_SETCW
#include <i386/fpu_control.h>
#define _FPU_SETCW(cw) __setfpucw(cw)
#endif
]], [[ unsigned short mode = 0 ; _FPU_SETCW(mode); ]])],[ac_cv_c_fpu_setcw="yes"],[ac_cv_c_ieee_interface=unknown])
    ])
fi

ac_tr_ieee_interface=HAVE_`echo $ac_cv_c_ieee_interface | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`_IEEE_INTERFACE
AC_DEFINE_UNQUOTED($ac_tr_ieee_interface,1,[IEEE Interface Type])

AC_SUBST(HAVE_GNUSPARC_IEEE_INTERFACE)
AC_SUBST(HAVE_GNUM68K_IEEE_INTERFACE)
AC_SUBST(HAVE_GNUPPC_IEEE_INTERFACE)
AC_SUBST(HAVE_GNUX86_IEEE_INTERFACE)
AC_SUBST(HAVE_SUNOS4_IEEE_INTERFACE)
AC_SUBST(HAVE_SOLARIS_IEEE_INTERFACE)
AC_SUBST(HAVE_HPUX11_IEEE_INTERFACE)
AC_SUBST(HAVE_HPUX_IEEE_INTERFACE)
AC_SUBST(HAVE_TRU64_IEEE_INTERFACE)
AC_SUBST(HAVE_IRIX_IEEE_INTERFACE)
AC_SUBST(HAVE_AIX_IEEE_INTERFACE)
AC_SUBST(HAVE_FREEBSD_IEEE_INTERFACE)
AC_SUBST(HAVE_OS2EMX_IEEE_INTERFACE)
AC_SUBST(HAVE_NETBSD_IEEE_INTERFACE)
AC_SUBST(HAVE_OPENBSD_IEEE_INTERFACE)
AC_SUBST(HAVE_DARWIN_IEEE_INTERFACE)
AC_SUBST(HAVE_DARWIN86_IEEE_INTERFACE)

dnl Check for IEEE control flags

save_cflags="$CFLAGS"
AC_CACHE_CHECK([for IEEE compiler flags], ac_cv_c_ieee_flags,
[
case "$host" in
    alpha*-*-*)
        if test X"$GCC" = Xyes ; then
            ieee_flags='-mieee -mfp-rounding-mode=d'
        else
            # This assumes Compaq's C compiler.
            ieee_flags='-ieee -fprm d'
        fi
        ;;
esac
if test X"$ieee_flags" != X ; then
  CFLAGS="$ieee_flags $CFLAGS"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int foo;]])],[ac_cv_c_ieee_flags="$ieee_flags"],[ac_cv_c_ieee_flags="none"])
else
  ac_cv_c_ieee_flags="none"
fi])

if test "$ac_cv_c_ieee_flags" != "none" ; then
   CFLAGS="$ac_cv_c_ieee_flags $save_cflags"
else
   CFLAGS="$save_cflags"
fi

dnl Check IEEE comparisons, whether "x != x" is true for NaNs
dnl
AC_CACHE_CHECK([for IEEE comparisons], ac_cv_c_ieee_comparisons,
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <math.h>
int main (void) 
{ 
   int status; double inf, nan;
   inf = exp(1.0e10);
   nan = inf / inf ;
   status = (nan == nan);
   exit (status);
}]])],[ac_cv_c_ieee_comparisons="yes"],[ac_cv_c_ieee_comparisons="no"],[ac_cv_c_ieee_comparisons="yes"])
])

if test "$ac_cv_c_ieee_comparisons" != no ; then
  AC_DEFINE(HAVE_IEEE_COMPARISONS,1)
  AC_SUBST(HAVE_IEEE_COMPARISONS)
fi

dnl Check for IEEE denormalized arithmetic
dnl
AC_CACHE_CHECK([for IEEE denormalized values], ac_cv_c_ieee_denormals,
[AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <math.h> 
int main (void) 
{ 
   int i, status; 
   volatile double z = 1e-308;
   for (i = 0; i < 5; i++) { z = z / 10.0 ; };        
   for (i = 0; i < 5; i++) { z = z * 10.0 ; };
   status = (z == 0.0);
   exit (status);
}]])],[ac_cv_c_ieee_denormals="yes"],[ac_cv_c_ieee_denormals="no"],[ac_cv_c_ieee_denormals="yes"])
])

if test "$ac_cv_c_ieee_denormals" != no ; then
  AC_DEFINE(HAVE_IEEE_DENORMALS,1)
  AC_SUBST(HAVE_IEEE_DENORMALS)
fi

dnl
AC_CONFIG_FILES([gsl-config gsl.pc gsl_version.h gsl.spec gsl/Makefile test/Makefile err/Makefile sys/Makefile utils/Makefile const/Makefile min/Makefile multimin/Makefile ieee-utils/Makefile fft/Makefile specfunc/Makefile dht/Makefile fit/Makefile multifit/Makefile bspline/Makefile statistics/Makefile sum/Makefile roots/Makefile multiroots/Makefile ntuple/Makefile poly/Makefile qrng/Makefile rng/Makefile randist/Makefile siman/Makefile integration/Makefile interpolation/Makefile doc/Makefile block/Makefile vector/Makefile matrix/Makefile histogram/Makefile monte/Makefile ode-initval/Makefile cblas/Makefile blas/Makefile linalg/Makefile eigen/Makefile permutation/Makefile combination/Makefile sort/Makefile complex/Makefile diff/Makefile deriv/Makefile cheb/Makefile cdf/Makefile wavelet/Makefile Makefile])
AC_OUTPUT
