dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(itcl, 3.1.0, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([tcl.m4])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

dnl    Define subdirectories which need to be recursed into
itcl_subdirs="itcl itk iwidgets2.2.0 iwidgets3.0.0"
AC_SUBST(itcl_subdirs)

dnl    To configure iTcl, run ./configure in the subdirectories.
(
    for f in itcl itk iwidgets2.2.0 iwidgets3.0.0
    do
       echo ./configure --prefix=$prefix --cache-file=config.cache
       ./configure --prefix=$prefix --cache-file=config.cache
    done
)

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
