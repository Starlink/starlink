dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(init, 1.1-26, ussc@star.rl.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR([cshrc.in])
STAR_DEFAULTS
STAR_DECLARE_DEPENDENCIES(build, [hlp])
STAR_CHECK_PROGS(hlib)
STAR_HELP_FILES(adam_packages.shl)

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
