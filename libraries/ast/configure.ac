dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   This configure file is known to work with autoconf-2.57,
dnl   automake versions 1.6.3 and 1.7.5, and libtool versions 1.4.2 and
dnl   1.5.  It should work with autoconf versions 2.50 or better, and
dnl   automake 1.6 or better.

dnl   Initialisation: package name and version number
AC_INIT(ast, 3.3, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([ast_link.in])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

dnl   Checks for programs
AC_PROG_CC
AC_PROG_CPP
AC_PROG_FC
AC_PROG_LIBTOOL
AC_F77_LIBRARY_LDFLAGS
AC_PROG_LN_S

dnl   Find an absolute path to the Perl binary, augmenting the path with the
dnl   location of the Starlink Perl build.  If this fails, then set @PERL@
dnl   to the backup `/usr/bin/env perl', which will find Perl if it exists
dnl   in the path at runtime.
AC_PATH_PROG(PERL, perl, [/usr/bin/env perl], [$STARLINK/Perl/bin:$PATH])

dnl   Function and declaration checks
AC_CHECK_FUNCS([isnan])
AC_CHECK_DECLS([isnan],,,[#include <math.h>
                         ])


if test -f sun210.tar_htx; then
    ast_sourceset_complete=true
else
    ast_sourceset_complete=false
fi


STAR_DECLARE_DEPENDENCIES(sourceset, sst)
STAR_DECLARE_DEPENDENCIES(use, sla)

dnl   Declare the message file.  This macro is documented to leave the
dnl   discovered facility code in the shell variable
dnl   $star_facilitycode, so substitute this (into error.h).
STAR_MESSGEN(ast_err.msg)
AC_SUBST(star_facilitycode)

dnl   Declare the documentation.  We need to do complicated things to
dnl   satisfy these targets, so give a non-null value
dnl   for the second argument, to suppress automatic generation of
dnl   rules.
STAR_LATEX_DOCUMENTATION([sun210 sun211], [sun210.htx_tar sun211.htx_tar])
STAR_CHECK_PROGS(star2html)

if $ast_sourceset_complete; then
    : do nothing
else
    dnl   Find prolat -- part of SST
    STAR_CHECK_PROGS(prolat, sst)
fi


AC_CONFIG_HEADERS(config.h)

AC_CONFIG_FILES(Makefile component.xml ast_link)
AC_CONFIG_FILES([ast_cpp], [chmod +x ast_cpp])

if $ast_sourceset_complete; then
    : do nothing
else
    AC_CONFIG_FILES(error.h version.h)
    AC_CONFIG_FILES(builddocs, [chmod +x builddocs])
    AC_CONFIG_FILES(addversion, [chmod +x addversion])
fi

AC_OUTPUT
