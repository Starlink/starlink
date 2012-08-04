dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(tcllibs, 1.0.0, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([tcllibs/README])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

STAR_DECLARE_DEPENDENCIES([build], [tcl])

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

STAR_SPECIAL_INSTALL_COMMAND([
   ( cd tcllibs; $(MAKE) DESTDIR=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml tcllibs/Makefile])

AC_OUTPUT
