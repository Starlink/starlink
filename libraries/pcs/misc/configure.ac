dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(misc, 1.4, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([misc_link_adam])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [cnf])
dnl    No use dependencies

dnl    Function check
AC_CHECK_FUNCS([atexit on_exit])

# Check for curses.h
AC_CHECK_HEADERS([curses.h cursesX.h])

dnl    Or ncurses term.h in /usr/include.
AC_CHECK_HEADERS([term.h ncurses/term.h])

dnl    Documented in SUN/144 in the parent directory

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
