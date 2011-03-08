dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(sst, 1.1-2, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([SST_PAR])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
AC_PROG_LN_S

dnl    Check if sys/wait.h exists and is POSIX compatible
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_FUNCS(memcpy)

STAR_MONOLITHS
STAR_CHECK_PROGS(ifd2star)
STAR_CHECK_PROGS(hlib)

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [chr fio hds hlp mers one par pcs psx sae ifd],
                          [link])
STAR_DECLARE_DEPENDENCIES([build], [pcs], [use])

STAR_LATEX_DOCUMENTATION(sun110)

AC_CONFIG_FILES(Makefile component.xml sst.ifd)
AC_CONFIG_HEADERS(config.h)

AC_OUTPUT
