dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number, this isn't actually
dnl    specx of course, but we need to declare it as such so it gets installed
dnl    in the correct place if we turn on per-package-dirs
AC_INIT(specx, 6.8-5, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([specx.hlp])

dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    We're building a help library here, so tell the configure system 
dnl    about it so it'll build and install correctly
STAR_HELP_FILES(photomhelp.shl)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
