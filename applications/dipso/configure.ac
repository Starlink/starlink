dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number.
AC_INIT(dipso, 3.6-4, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([dipso.f])
 
dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [ast cat kaplibs snx pda ndf sae])

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_FC_LIBRARY_LDFLAGS

dnl    Documentation.
STAR_LATEX_DOCUMENTATION(sun50, [sun50.ps sun50.htx_tar sun50.tex])
STAR_CHECK_PROGS(star2html)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml my_dipso])

dnl    This is the bit that does the actual work
AC_OUTPUT
