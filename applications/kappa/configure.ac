dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(kappa, 1.4, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([kappa_mon.f.in])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CNF_COMPATIBLE_SYMBOLS

STAR_CHECK_PROGS([ifd2star awish atclsh hlib generic])
STAR_CHECK_PROGS(prohlp,sst)

dnl    Look for tcl/tk system
STAR_PATH_TCLTK(8.2, [tk])
AC_CHECK_HEADERS([tcl.h tk.h])

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [fio prm par sae shl hds kaplibs ard mag pgplot tcl tk])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun95 sun221)


AC_CONFIG_FILES([Makefile component.xml 
    kappa_mon.f ndfpack_mon.f kapview_mon.f kappa.ifd
    atools/Makefile   kapsub/Makefile    libkapview/Makefile
    ccdpack/Makefile  libkappa/Makefile  libndfpack/Makefile])
AC_CONFIG_FILES(kappa_link_adam,  [chmod +x kappa_link_adam])
AC_CONFIG_FILES(lutedit.tcl,      [chmod +x lutedit.tcl])
AC_CONFIG_FILES(adamMessageRelay, [chmod +x adamMessageRelay])
AC_CONFIG_FILES(stardemo.tcl,     [chmod +x stardemo.tcl])

AC_OUTPUT
