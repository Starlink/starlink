dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(iras90, 1.1-2, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([iras90_pm.f])

STAR_DEFAULTS(per-package-dirs)

STAR_MONOLITHS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS([hlib])
STAR_CHECK_PROGS([ifd2star])

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [ard agi fio snx ndf ary sla])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun161 sun163 sun165)
STAR_DOCS_FILES(sun161_a_1_1.ps  sun161_a_3_1_canon.dat  sun161_a_4_3.ps sun161_a_1_2.ps  sun161_a_4_1.ps sun161_a_4_4.ps sun161_a_2_1.ps  sun161_a_4_2.ps sun161_a_4_5.ps)


AC_CONFIG_SUBDIRS([i90 ira irc iri irq irm apps])

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
