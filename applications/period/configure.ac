dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(period, 5.0-3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([period_main.f])

STAR_DEFAULTS()

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [sae psx cfitsio pgplot mers ], [link])
STAR_DECLARE_DEPENDENCIES(sourceset, star2html)

STAR_LATEX_DOCUMENTATION([sun167 ssn25 period_history])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
