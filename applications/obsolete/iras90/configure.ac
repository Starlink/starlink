dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(iras90, 1.1-2, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([iras90_pm.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL
AC_PROG_LN_S
STAR_MONOLITHS

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS([hlib ifd2star])

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build],[ard agi fio ifd snx ndf ndg ary sla shl],[link])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun161 sun163 sun165)

AC_CONFIG_FILES([Makefile component.xml
    apps/Makefile  ira/Makefile  iri/Makefile  irq/Makefile
    i90/Makefile   irc/Makefile  irm/Makefile
    ])

AC_OUTPUT
