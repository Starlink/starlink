dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    Note that for GKS the RAL version number if 1.37 but
dnl    Starlink does not increment that version. Rather than use
dnl    non-standard nomenclature use the patch level for starlink patching.
AC_INIT(gks-data, 1.37-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS


dnl    Find required versions of the programs we need for configuration
dnl AC_PROG_CC
dnl AC_PROG_FC
dnl AC_PROG_LIBTOOL
AC_PROG_MAKE_SET

AC_CONFIG_SUBDIRS( fonts wdts emf )

AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
