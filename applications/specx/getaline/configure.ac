dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(getaline, 1.2-1, rpt@jach.hawaii.edu)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([get_aline.c])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

AC_CHECK_HEADERS([readline/readline.h])
AC_CHECK_HEADERS([readline/history.h])
AC_CHECK_FUNCS([rindex index strchr strrchr])

dnl    Note that we readline currently depends on termcap
dnl    so to build with readline we need to find something in termcap
dnl    Do not know what happens if this all changes
AC_CHECK_LIB([termcap],[tgetnum])

dnl    Need to find readline library
dnl    Could do with a --with-readline option
AC_CHECK_LIB([readline],[readline])
AC_CHECK_LIB([history],[add_history])
AC_CHECK_FUNCS([readline add_history])

STAR_CNF_COMPATIBLE_SYMBOLS

AM_CONFIG_HEADER(config.h)

dnl    This sub-package has no dependencies
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([getaline_link],[chmod +x getaline_link])

AC_OUTPUT
