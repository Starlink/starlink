dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(adam, 3.0, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([ADAM_DEFNS])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [sae messys])
STAR_DECLARE_DEPENDENCIES(link,  [chr psx ems messys])

dnl    Documentation is in the parent directory
dnl STAR_LATEX_DOCUMENTATION(sun114)

dnl    There is no .msg file in this directory.  The ADAM_ERR file 
dnl    contains only a single definition, of the parameter ADAM__OK.

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
