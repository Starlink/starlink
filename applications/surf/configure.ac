dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(surf, 1.6-12, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([surf_mon.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CHECK_PROGS( ifd2star )

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [sae prm pcs mers ndf])
STAR_DECLARE_DEPENDENCIES([link], [ndf fio sla hlp pda prm agi kaplibs pgplot grp ndg])
dnl    No use dependencies

dnl STAR_LATEX_DOCUMENTATION(sun102)

AC_CONFIG_SUBDIRS([sculib surflib src])

AC_CONFIG_FILES(Makefile component.xml surf_set_app_name.f surf.ifd)

AC_OUTPUT
