dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number, this isn't actually
dnl    datacube of course, but we need to declare it as such so it gets
dnl    installed in the correct place.
AC_INIT(datacube, 1.0-4, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([datacube_help.f])

dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS(per-package-dirs)
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_MONOLITHS

dnl    Conditional compile for missing symbols on Mac OSX. Using this in
dnl    an odd way, essentially to check whether we're on a Mac. If we are
dnl    we need to link against this library, it will be included by default
dnl    if it exists in this case.
AC_CHECK_LIB(cc_dynamic, restFP)

dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    We're building a help library here, so tell the configure system 
dnl    about it so it'll build and install correctly
STAR_HELP_FILES(datacube.shl)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
