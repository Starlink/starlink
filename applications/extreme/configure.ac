dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(scb, 0.1-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([do-xxx])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PATH_PROG(PERL, perl)

AM_PROG_LEX

STAR_LATEX_DOCUMENTATION(ssn73)

AC_CONFIG_FILES([Makefile component.xml extreme.csh])
AC_CONFIG_FILES(cmp-xxx, [chmod +x cmp-xxx]) 
AC_CONFIG_FILES(extmakefile, [chmod +x extmakefile])
AC_CONFIG_FILES(extmk, [chmod +x extmk])

dnl    This is the bit that does the actual work
AC_OUTPUT
