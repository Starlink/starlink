dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 19893 $)

dnl    Initialisation: package name and version number
AC_INIT(gsl, 1.15, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([gsl/configure.ac])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

dnl    GSL likes a C99 compiler to handle the inlining of functions.
dnl    When we have a C99 compiler that isn't GCC we need to define
dnl    GSL_C99_INLINE to make this work.
AC_PROG_CC_C99
if test "$ac_cv_prog_cc_c99" != "no" -a "$GCC" != "yes"; then
   CC="$CC -DGSL_C99_INLINE"
fi

dnl    To configure gsl, run ./configure in the subdirectory.

#  Look for --disable-shared and pass it on.
star_extra_items=
for ac_arg in $ac_configure_args; do
   case $ac_arg in 
      *-disable-shared*)
         star_extra_items="--disable-shared"
      ;;
   esac
done
export star_extra_items
(
   cd gsl; \
   cmd="./configure --prefix=$prefix \
                    --srcdir=. \
                    $star_extra_items \
                    --cache-file=../config.cache"; \
   echo $cmd; \
   $cmd; \
)
           
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd gsl; $(MAKE) INSTALL_ROOT=$$DESTDIR DESTDIR=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
