dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 16836 $)

dnl    Initialisation: package name and version number
AC_INIT(starbench, 1.4-2, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sysinfo.c])

STAR_DEFAULTS(per-package-dirs)

STAR_MONOLITHS

STAR_CHECK_PROGS(fgeneric)
STAR_CHECK_PROGS(ifd2star)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([use], [daophot ccdpack pisa figaro ], [link])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(ssn23)

AC_CONFIG_FILES([Makefile component.xml slatest/Makefile starbench.ifd ])

AC_OUTPUT
