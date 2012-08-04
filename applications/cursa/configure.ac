dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(cursa, 6.5, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([cursa.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CHECK_PROGS([starperl])
STAR_CHECK_PROGS([awish])
STAR_CHECK_PROGS([atclsh])

dnl    Extra libraries needed to link against sockets on this system.
dnl    These tests are definitely needed for Solaris.
AC_CHECK_FUNC(gethostbyname,,
  AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(socket,,
  AC_CHECK_LIB(socket, connect,,,-lnsl))

AC_SEARCH_LIBS(inet_ntop, nsl,[
    AC_DEFINE(HAVE_INET_NTOP, 1,
    [Define to 1 if you have the `inet_ntop' function.])])
AC_SEARCH_LIBS(inet_pton, nsl,[
    AC_DEFINE(HAVE_INET_PTON, 1,
    [Define to 1 if you have the `inet_pton' function.])])
AC_CHECK_FUNCS([memset bzero])

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [sae cat fio par pda ndf pgplot startcl], [link])
dnl    cursa_link_adam depends on pgplot_link_adam which is, oddly
dnl    enough, in graphpar rather than pgplot.
STAR_DECLARE_DEPENDENCIES([build], [graphpar])
STAR_DECLARE_DEPENDENCIES([sourceset], [star2html])

dnl    The starperl command is part of perl.
STAR_DECLARE_DEPENDENCIES([configure], [perl])

STAR_LATEX_DOCUMENTATION([sun190 ssn76])

AC_CONFIG_FILES([Makefile component.xml libcap/Makefile])

AC_OUTPUT
