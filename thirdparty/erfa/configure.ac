dnl    Process this file with autoconf to produce a configure script

dnl    This is not a package, so the first two arguments are fake.
AC_INIT(erfa, 1.3.0, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Ensure that we have the submodule
AC_CONFIG_SRCDIR([erfa/configure.ac])

STAR_DEFAULTS

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

dnl    To configure erfa, run ./configure in the subdirectory.

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
   cd erfa; \
   cmd="./bootstrap.sh"; \
   echo $cmd; \
   $cmd; \
   cmd="./configure --prefix=$prefix \
                    --srcdir=. \
                    $star_extra_items \
                    --cache-file=../config.cache"; \
   echo $cmd; \
   $cmd; \
)

STAR_SPECIAL_INSTALL_COMMAND([
   ( cd erfa; $(MAKE) INSTALL_ROOT=$$DESTDIR DESTDIR=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
