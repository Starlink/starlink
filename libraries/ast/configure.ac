dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   This configure file is known to work with autoconf-2.57,
dnl   automake versions 1.6.3 and 1.7.5, and libtool versions 1.4.2 and
dnl   1.5.  It should work with autoconf versions 2.50 or better, and
dnl   automake 1.6 or better.

dnl   Initialisation: package name and version number
AC_INIT(ast, 3.5, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([ast_link.in])

#   Include defaults for Starlink configurations
STAR_DEFAULTS

#   Checks for programs
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LIBTOOL
AC_PROG_LN_S

#   ast_link needs to be able to link against the Fortran runtime if
#   necessary
AC_FC_LIBRARY_LDFLAGS

#   Find an absolute path to the Perl binary, augmenting the path with the
#   location of the Starlink Perl build.  If this fails, then set @PERL@
#   to the backup `/usr/bin/env perl', which will find Perl if it exists
#   in the path at runtime.
AC_PATH_PROG(PERL, perl, [/usr/bin/env perl], [$STARLINK/Perl/bin:$PATH])

#   Function and declaration checks
AC_CHECK_FUNCS([isnan])
AC_CHECK_DECLS([isnan],,,[#include <math.h>
                         ])
STAR_DECLARE_DEPENDENCIES(sourceset, [sst htx])
STAR_DECLARE_DEPENDENCIES(build, sla)

#   Declare the message file.  
STAR_MESSGEN(ast_err.msg)

#   Declare the documentation.  We need to do complicated things to
#   satisfy these targets, so give a non-null value
#   for the second argument, to suppress automatic generation of
#   rules.
STAR_LATEX_DOCUMENTATION([sun210 sun211], [sun210.htx_tar sun211.htx_tar])
STAR_PREDIST_SOURCES(sun_master.tex)
STAR_CHECK_PROGS(star2html)
STAR_CHECK_PROGS(prolat, sst)   # prolat is part of SST

AC_CONFIG_HEADERS(config.h)

AC_CONFIG_FILES(Makefile component.xml ast_link)
AC_CONFIG_FILES([ast_cpp], [chmod +x ast_cpp])
#   Following are files which are substituted by the Makefile at
#   distribution time, rather than by configure.  They are not distributed.
STAR_PREDIST_SOURCES(version.h.in builddocs.in addversion.in)

AC_OUTPUT
