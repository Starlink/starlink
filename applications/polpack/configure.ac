dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([polpack],[3.4-0],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([polpack_mon.f.in])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
LT_INIT

STAR_MONOLITHS
STAR_CHECK_PROGS([ifd2star awish atclsh hlib])
STAR_CHECK_PROGS(prohlp,sst)

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Look for tck/tk system
STAR_PATH_TCLTK(8.5, [tcl])

dnl POLPACK source files depend on include files from the following components
STAR_DECLARE_DEPENDENCIES([build], [ast cat chr cnf hds fio grp mers ndf ndg par prm sae startcl tcl trn ifd])

dnl The POLPACK link script links against the following components
STAR_DECLARE_DEPENDENCIES([build], [sla kaplibs ctg shl tcl], [link])

dnl Using POLPACK requires the following components to be available at run-time.
STAR_DECLARE_DEPENDENCIES([use], [kappa ccdpack tcl tk startcl])

STAR_LATEX_DOCUMENTATION(sun223)
STAR_PREDIST_SOURCES(polka.tex)

AC_CONFIG_FILES([Makefile component.xml polpack_mon.f polpack.ifd
    ccdpack/Makefile polsub/Makefile polsub/pol1_ptvrc.f polsub/pol1_ptvrs.f])
AC_CONFIG_FILES(polpack_link_adam, [chmod +x polpack_link_adam])
AC_CONFIG_FILES(Polka.tcl, [chmod +x Polka.tcl])

AC_OUTPUT
