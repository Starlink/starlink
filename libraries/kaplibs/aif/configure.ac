dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
AC_INIT(kaplibs-aif, 2.7-3, ussc@star.rl.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([aif_antmp.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

dnl    Other options here, such as
dnl STAR_CNF_COMPATIBLE_SYMBOLS

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile])

dnl    This is the bit that does the actual work
AC_OUTPUT
