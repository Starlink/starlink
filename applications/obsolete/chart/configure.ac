dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(chart, 4.0-1, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([chart_mon.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
AC_PROG_LN_S

STAR_MONOLITHS
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Open specifiers
AC_FC_OPEN_SPECIFIERS(readonly)
AC_FC_OPEN_SPECIFIERS(action='read')
AC_FC_OPEN_SPECIFIERS(carriagecontrol='LIST')

dnl    What unit does OPEN's recl specifier assume?
AC_FC_RECL_UNIT


STAR_CHECK_PROGS([ifd2star hlib csh])

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [chr mers sgs gks ifd graphpar pda], [link])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun32)

AC_CONFIG_FILES([Makefile component.xml chart.ifd
    libchart/Makefile applic/Makefile])

AC_OUTPUT
