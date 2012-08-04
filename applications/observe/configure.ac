dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(observe, 2.3-2, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sun146.tex])

STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_MONOLITHS

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES(build, [par sae])
STAR_DECLARE_DEPENDENCIES(build, [chr fio mers par pgp sla], link)
STAR_DECLARE_DEPENDENCIES(sourceset, star2html)

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    Documentation.
STAR_LATEX_DOCUMENTATION(sun146)

dnl    Configure files.
AC_CONFIG_FILES([Makefile component.xml])

dnl    The fat lady sings.
AC_OUTPUT
