dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(dtask, 3.0, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([DTASK_PAR])

# Include defaults for Starlink configurations
STAR_DEFAULTS

# Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

AC_FC_LIBRARY_LDFLAGS

AC_PATH_PROGS(M4, m4, m4)

STAR_CNF_COMPATIBLE_SYMBOLS

AC_FC_CHECK_INTRINSICS([GETCWD CHDIR PUTENV])

STAR_DECLARE_DEPENDENCIES([build], [adam cnf messys psx sae subpar])
STAR_DECLARE_DEPENDENCIES([link],  [chr ems mers ams atimer hds psx subpar])
STAR_DECLARE_DEPENDENCIES([use],   [hdspar])

STAR_MESSGEN(act_err.msg dtask_err.msg)

dnl    We need some further configuration: see the original dtask
dnl    makefile, which edits in extra libraries on Solaris (such as
dnl    -lsocket); see the installs of LINK_SCRIPTS in that makefile.
dnl    This is probably part of a more general bit of configuration
dnl    necessary on Solaris.

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES(Makefile component.xml)

dnl    Files alink.in and ilink.in are substituted at `make' time,
dnl    not configure time.

dnl    This is the bit that does the actual work
AC_OUTPUT
