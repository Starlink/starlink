dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 19893 $)

dnl    Initialisation: package name and version number
AC_INIT([automake],[starlink-1.15],[starlink@jiscmail.ac.uk])

dnl    Require autoconf-2.69 at least
AC_PREREQ([2.69])
dnl    Require a Starlink automake.
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([automake/configure.ac])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

dnl    To configure autoconf, run ./configure in the subdirectory.

#  Look for --disable-shared and pass it on.
(
   cd automake; \
   cmd="./configure --prefix=$prefix \
                    --srcdir=. \
                    --cache-file=../config.cache"; \
   echo $cmd; \
   $cmd; \
)
           
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd automake; $(MAKE) INSTALL_ROOT=$$DESTDIR DESTDIR=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
