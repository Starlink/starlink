dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    Note that for GKS the RAL version number if 1.37 but
dnl    Starlink does not increment that version. Rather than use
dnl    non-standard nomenclature use the patch level for starlink patching.
AC_INIT(pgplot, 5.2.2-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
dnl AC_CONFIG_SRCDIR([pgplot_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [cnf gwm psx mers chr])
STAR_DECLARE_DEPENDENCIES([link],  [gwm cnf psx mers chr])


dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])

dnl    Need to go into sub directories
AC_CONFIG_SUBDIRS([src sys drivers fonts cpg pgdispd ])

dnl    This is the bit that does the actual work
AC_OUTPUT
