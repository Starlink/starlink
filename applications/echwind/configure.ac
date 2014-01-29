dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([echwind],[3.1-4],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([echwind.F])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
LT_INIT

dnl    Need to check for ACCESS mode
AC_FC_OPEN_SPECIFIERS(access='append')
AC_FC_OPEN_SPECIFIERS(position='append')


STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [pgplot starx])
dnl    No use dependencies

STAR_LATEX_DOCUMENTATION(sun53)

AC_CONFIG_FILES([Makefile component.xml
    libetc/Makefile libmodel/Makefile libplot/Makefile])

AC_CONFIG_HEADERS(config.h)

AC_OUTPUT
