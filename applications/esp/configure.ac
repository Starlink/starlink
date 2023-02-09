dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT([esp],[0.11-4],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR([ELP_PAR])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
dnl AC_PROG_LIBTOOL
STAR_MONOLITHS

LT_INIT

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS( hlib )

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build],
   [agi ast ard cat chr fio grp hds kaplibs ndf mers par pcs pda prm psx sae shl sgs],
   [link])

STAR_LATEX_DOCUMENTATION(sun180)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile component.xml esp1_vers.f])
AC_OUTPUT
