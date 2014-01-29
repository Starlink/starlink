dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([kaprh],[2.0],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([kaprh_mon.f.in])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
LT_INIT

STAR_MONOLITHS

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS([ifd2star hlib fgeneric])
STAR_CHECK_PROGS(prohlp,sst)

dnl KAPRH source files depend on include files from the following components  
STAR_DECLARE_DEPENDENCIES([build], [ast fio gks gns hds idi kaplibs mers ndf par pcs prm sae trn ifd generic])

dnl The KAPRH link script links against the following components
STAR_DECLARE_DEPENDENCIES([build], [agi ard cat fio gks gns grp gwm idi kaplibs ncar ndf ndg pda prm psx ref sgs shl sla snx trn], [link])

dnl We use prohlp from sst
STAR_DECLARE_DEPENDENCIES([sourceset], [sst])

STAR_LATEX_DOCUMENTATION(sun239)

AC_CONFIG_FILES(Makefile component.xml kaprh_mon.f kaprh.ifd kapsub/Makefile)

AC_OUTPUT
