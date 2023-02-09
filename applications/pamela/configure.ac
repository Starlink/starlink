dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([pamela],[1.0.6],[t.r.marsh@warwick.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([pamela_mon.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
LT_INIT

STAR_MONOLITHS

STAR_CNF_COMPATIBLE_SYMBOLS

STAR_CHECK_PROGS([fgeneric])
STAR_CHECK_PROGS(ifd2star)

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [fio generic prm par sae hds ndf], [link])
dnl    No use dependencies

dnl    Need "hlib" command to build the help library.
dnl STAR_CHECK_PROGS(hlib)

dnl STAR_LATEX_DOCUMENTATION(sun102)

AC_CONFIG_FILES([Makefile pamela.ifd src/Makefile component.xml])

AC_OUTPUT
