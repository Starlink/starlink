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

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS([ifd2star hlib fgeneric])
STAR_CHECK_PROGS(prohlp,sst)

dnl KAPRH source files depend on include files from the following components  
STAR_DECLARE_DEPENDENCIES([build], [ast kaplibs hds fio gks gns idi mers ndf par prm sae subpar trn])

dnl The KAPRH link script links against the following components
STAR_DECLARE_DEPENDENCIES([build], [kaplibs cat snx ncar idi sgs ndg gks gwm gns ndf agi fio ref ard grp trn prm shl mag sla psx pda], [link])

STAR_LATEX_DOCUMENTATION(sun239)

AC_CONFIG_FILES(Makefile component.xml kaprh_mon.f kaprh.ifd kapsub/Makefile)

AC_OUTPUT
