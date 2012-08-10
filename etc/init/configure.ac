dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(init, 1.1-27, starlink@jiscmail.ac.uk)
AC_PREREQ(2.50)

dnl    Under OS X LD_LIBRARY_PATH is DYLD_LIBRARY_PATH.
AC_CANONICAL_TARGET
case "$target_os" in
   *darwin*)
      AC_SUBST([LD_LIB_PATH],[DYLD_LIBRARY_PATH])
   ;;
   *)
      AC_SUBST([LD_LIB_PATH],[LD_LIBRARY_PATH])
   ;;
esac

AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR([cshrc.in])
STAR_DEFAULTS
STAR_DECLARE_DEPENDENCIES(build, [hlp])
STAR_CHECK_PROGS(hlib)

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
