dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(psx, 0.5, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([psx_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC

AC_HEADER_STDC
AC_CHECK_HEADERS(unistd.h sys/types.h)

AC_CHECK_FUNCS([getpwuid geteuid getlogin cuserid])
AC_CHECK_DECLS([cuserid])
AC_CHECK_FUNCS([setenv])
AC_CHECK_FUNCS([getenv])
AC_CHECK_FUNCS([uname])
AC_CHECK_FUNCS([getppid])
AC_CHECK_FUNCS([getgid])
AC_CHECK_FUNCS([getegid])
AC_CHECK_FUNCS([getuid])
AC_CHECK_FUNCS([ttyname])
AC_CHECK_FUNCS([ctime ctime_r])
AC_CHECK_FUNCS([gmtime gmtime_r])
AC_CHECK_FUNCS([localtime localtime_r])
AC_CHECK_FUNCS([asctime asctime_r])

dnl    MinGW Windows libraries and functions. Checking for the functions,
dnl    fails (not enough like UNIX) so must check for the declaration only.
AC_CHECK_DECLS([GetUserName],,,[#include <windows.h>])

AC_PROG_LIBTOOL

dnl    If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae mers])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun121)

STAR_MESSGEN(psx_err.msg)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
