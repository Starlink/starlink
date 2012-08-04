dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 19893 $)

dnl    Initialisation: package name and version number
AC_INIT(fftw, 3.1.2, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([fftw/configure.ac])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

dnl    Conditional defining whether we build with POSIX thread support.
dnl    We do this by default.
star_extra_items=
AC_ARG_WITH([pthreads],
            [ --with-pthreads   Build package with POSIX threads support],
            [if test "$withval" = "yes"; then
               use_pthreads="yes"
             else
               use_pthreads="no"
             fi],
            [use_pthreads="yes"])
if test "$use_pthreads" = "yes"; then
   AC_CHECK_LIB([pthread], [pthread_create], ,[use_pthreads="no"])
   if test "$use_pthreads" = "yes"; then
      star_extra_items="--enable-threads"
   fi
fi

dnl    To configure fftw, run ./configure in the subdirectory.

#  Look for --disable-shared and pass it on.
for ac_arg in $ac_configure_args; do
   case $ac_arg in 
      *-disable-shared*)
         star_extra_items="$star_extra_items --disable-shared"
      ;;
   esac
done
export star_extra_items
(
   cd fftw; \
   cmd="./configure --prefix=$prefix \
                    --srcdir=. \
                    $star_extra_items \
                    --cache-file=../config.cache"; \
   echo $cmd; \
   $cmd; \
)
           
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd fftw; $(MAKE) INSTALL_ROOT=$$DESTDIR DESTDIR=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
