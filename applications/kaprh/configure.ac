dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(kaprh, 2.0, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([kaprh_mon.f.in])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CHECK_PROGS(ifd2star)
STAR_CHECK_PROGS(prohlp,sst)
STAR_CHECK_PROGS(hlib)


dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [ast kaplibs hds fio gks gns idi mers ndf prm par sae trn subpar])

STAR_LATEX_DOCUMENTATION(sun239)

AC_CONFIG_SUBDIRS([kapsub])

AC_CONFIG_FILES(Makefile component.xml kaprh_mon.f kaprh.ifd)

AC_OUTPUT
