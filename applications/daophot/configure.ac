dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(daophot, 1.3-3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([daophot.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)
STAR_MONOLITHS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_DECLARE_DEPENDENCIES([build], [sae mers pcs ndf psx chr agi gks cnf], [link])
STAR_LATEX_DOCUMENTATION(sun42)

AC_CONFIG_FILES([Makefile component.xml daophot.csh])

AC_OUTPUT
