dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(jcmtdr, 1.2-3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([jcmtdr.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_FC_LIBRARY_LDFLAGS

STAR_CHECK_PROGS(ifd2star)
STAR_CHECK_PROGS(hlib)


dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [sla hlp ndf prm fio gsd pda figaro kaplibs])
dnl    No use dependencies

dnl STAR_LATEX_DOCUMENTATION(sun95 sun221)

AC_CONFIG_SUBDIRS([subs applic])

AC_CONFIG_FILES(Makefile component.xml jcmtdr.ifd)

AC_OUTPUT
