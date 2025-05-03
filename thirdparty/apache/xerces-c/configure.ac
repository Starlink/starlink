dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 19893 $)

dnl    Initialisation: package name and version number
AC_INIT(xerces-c, 3.3.0, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([xerces-c/configure.ac])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

dnl    To configure xerces-c, run ./configure in the subdirectory.

#  Look for --disable-shared and pass it on.
star_extra_items=
for ac_arg in $ac_configure_args; do
   case $ac_arg in 
      *-disable-shared*)
         star_extra_items="--disable-shared"
      ;;
   esac
done

dnl  CURL never seems to be picked up from the right place
dnl  or version, so dispense with it.
star_extra_items="$star_extra_items --disable-netaccessor-curl"
export star_extra_items
(
   cd xerces-c; \
   cmd="./configure --prefix=$prefix \
                    --srcdir=. \
                    $star_extra_items \
                    --cache-file=../config.cache"; \
   echo $cmd; \
   $cmd; \
)
           
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd xerces-c; $(MAKE) INSTALL_ROOT=$$DESTDIR DESTDIR=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
