dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([fluxes],[1.5-0],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([fluxes.c])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
LT_INIT

STAR_MONOLITHS
STAR_CHECK_PROGS(sh)

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae;
dnl    links to: chr, mers, fio, psx, task jpl.
STAR_DECLARE_DEPENDENCIES([build], [sae mers par pal erfa grp kaplibs ast ndf chr one])
STAR_DECLARE_DEPENDENCIES([link], [pal erfa mers])

dnl    No use dependencies

dnl Documentation
STAR_LATEX_DOCUMENTATION(sun213)

AC_CONFIG_FILES(Makefile component.xml libflu/Makefile)

AC_OUTPUT
