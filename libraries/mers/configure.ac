dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(mers, 2.1-0, starlink@jiscmail.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([MSG_PAR])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS
# STAR_MONOLITHS

AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL
AC_PROG_LN_S
AC_CHECK_FUNCS([strtok_r])

dnl   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

STAR_CNF_COMPATIBLE_SYMBOLS

STAR_DECLARE_DEPENDENCIES([build], [cnf ems starutil sae ])
STAR_DECLARE_DEPENDENCIES([link],  [chr ems starutil ])

dnl   There are two distinct sets of error codes for this package
STAR_MESSGEN(err_err.msg)
STAR_MESSGEN(msg_err.msg)

STAR_LATEX_DOCUMENTATION(sun104)

dnl   Header checks.  There are no such explicit checks at present,
dnl   only the implicit ones.  Also config.h is not at present
dnl   included by anything.  However we might as well include this
dnl   stanza here, to support that in future, but also to keep down
dnl   the length of generated command lines.
AM_CONFIG_HEADER(config.h)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
