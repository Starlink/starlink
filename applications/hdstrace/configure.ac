dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([hdstrace],[1.2-5],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([hdstrace.f])

STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
LT_INIT
STAR_MONOLITHS

STAR_CNF_COMPATIBLE_SYMBOLS

STAR_CHECK_PROGS([fgeneric])

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [fio generic one prm par sae hds], [link])
dnl    No use dependencies

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

STAR_LATEX_DOCUMENTATION(sun102)

AC_CONFIG_FILES([Makefile component.xml tra/Makefile])

AC_OUTPUT
