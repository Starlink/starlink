dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(sc2, 3, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR(sc2.tex)

STAR_DEFAULTS(docs-only)

STAR_LATEX_DOCUMENTATION(sc2)

AC_CONFIG_FILES(Makefile component.xml examples/Makefile)

AC_OUTPUT
