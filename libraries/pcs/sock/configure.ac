dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(sock, 1.0, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sock_link_adam.in])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_LIBTOOL

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae])
dnl    No use dependencies

dnl    Extra libraries needed to link against sockets on this system.
dnl    These tests are definitely needed for Solaris.
AC_CHECK_FUNC(gethostbyname,,
  AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(socket,,
  AC_CHECK_LIB(socket, connect,,,-lnsl))

dnl    Described in SSN/29

STAR_MESSGEN(sock_err.msg)

AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES([sock_link], [chmod 755 sock_link])
AC_CONFIG_FILES([sock_link_adam], [chmod 755 sock_link_adam])


AC_OUTPUT
