dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(polpack, 3.1-3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([polpack_mon.f.in])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CHECK_PROGS(ifd2star)
STAR_CHECK_PROGS(awish)
STAR_CHECK_PROGS(atclsh)
STAR_CHECK_PROGS(prohlp,sst)
STAR_CHECK_PROGS(hlib)

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [fio prm par sae hds kaplibs ard pgplot])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun223)

AC_CONFIG_SUBDIRS([polsub ccdpack])

AC_CONFIG_FILES([Makefile component.xml polpack_mon.f pol1_ptvrs.f pol1_ptvrc.f polpack.ifd])
AC_CONFIG_FILES(Polka.tcl, [chmod +x Polka.tcl])
AC_CONFIG_FILES(adamMessageRelay, [chmod +x adamMessageRelay])

AC_OUTPUT
