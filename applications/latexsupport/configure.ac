dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(latexsupport, 1.0, starlink@jiscmail.ac.uk)
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([starlink.cls])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)

AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
