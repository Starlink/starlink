dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(ircamdr, 1.1-2, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([io2ro.f])

STAR_DEFAULTS(per-package-dirs)

STAR_MONOLITHS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL
AC_PROG_LN_S

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [ndf sgs fio gks mag])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun41)

AC_CONFIG_SUBDIRS([aif aspker atlib goodies kergen kersub mapgl rapgen rapsub trace io2ro_f obsrap_f plt2d_f polrap_f rapi2d_f])

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
