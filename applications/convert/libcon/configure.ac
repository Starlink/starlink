dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(convert-libcon, 1.5-4, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([con_cclos.c])
 
dnl    Include defaults for Starlink configurations
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_CC
AC_PROG_LIBTOOL

dnl    Need generic.
STAR_CHECK_PROGS(generic)

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Build dependencies for this sub-package.
STAR_DECLARE_DEPENDENCIES([build], [ast hds grp mers ndf ndg prm par sae])

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
