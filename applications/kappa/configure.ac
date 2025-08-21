dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(kappa, 2.6-13, starlink@jiscmail.ac.uk)
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([kappa_mon.f.in])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
LT_INIT

STAR_MONOLITHS
STAR_CNF_COMPATIBLE_SYMBOLS

STAR_CHECK_PROGS(prohlp,sst)
STAR_CHECK_PROGS(ifd2star)
STAR_CHECK_PROGS(awish)
STAR_CHECK_PROGS(atclsh)
STAR_CHECK_PROGS(hlib)
STAR_CHECK_PROGS(fgeneric)
STAR_CHECK_PROGS(perl)

dnl    tk requires X11 so we need the X11 include path
AC_PATH_XTRA

dnl    Look for tcl/tk system
STAR_PATH_TCLTK(8.6, [tk])
AC_CHECK_HEADERS([tcl.h tk.h])

dnl KAPPA source files depend on include files from the following components
STAR_DECLARE_DEPENDENCIES([build], [agi ast chr cnf fio grp hds ifd kaplibs mers ndf one par pcs prm psx sae shl tcl tk trn])

dnl The KAPPA link script links against the following components
STAR_DECLARE_DEPENDENCIES([build], [ard atl kaplibs irq one shl], [link])

dnl We use the sst package to build documentation (prohlp)
STAR_DECLARE_DEPENDENCIES([sourceset], [sst])

dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun95)

dnl    There is a .msg file in this directory, and this adds Makefile
dnl    rules to generate the xxx_ERR, xxx_err.h and fac_nnn_err files
dnl    which correspond to it.
STAR_MESSGEN(kap_err.msg)

AC_CONFIG_FILES([Makefile component.xml kappa.news
    kappa_mon.f ndfpack_mon.f kapview_mon.f kappa.ifd
    kapsub/Makefile     libkapview/Makefile
    libkappa/Makefile   libndfpack/Makefile])
AC_CONFIG_FILES(kappa_link_adam,  [chmod +x kappa_link_adam])
AC_CONFIG_FILES(lutedit.tcl,      [chmod +x lutedit.tcl])
AC_CONFIG_FILES(stardemo.tcl,     [chmod +x stardemo.tcl])
AC_CONFIG_FILES(sort-help.pl,     [chmod +x sort-help.pl])

AC_OUTPUT
