dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(tclsys, 8.2.3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR([license.terms])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    To configure Tcl, run ./configure in the `unix' directory.
dnl    Do not invoke AC_CONFIG_SUBDIRS, since that prompts autoreconf
dnl    to try to reconfigure that directory, and automake to assume 
dnl    it's allowed to play there, too.
(
    cd unix
    ./configure --prefix=$prefix --cache-file=config.cache
)

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
