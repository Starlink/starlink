dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(ircamdr, 1.1-2, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([io2ro.f])

STAR_DEFAULTS(per-package-dirs)

STAR_MONOLITHS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL
AC_PROG_LN_S

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [ndf sgs fio gks graphpar mag pda], [link])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun41)

AC_CONFIG_FILES([Makefile component.xml ircamdr
    aif/Makefile      kersub/Makefile    rapgen/Makefile
    aspker/Makefile   mapgl/Makefile     rapi2d_f/Makefile
    atlib/Makefile    misc/Makefile      rapsub/Makefile
    goodies/Makefile  obsrap_f/Makefile  trace/Makefile
    io2ro_f/Makefile  plt2d_f/Makefile
    kergen/Makefile   polrap_f/Makefile])

AC_OUTPUT
