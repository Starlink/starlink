dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
dnl    Call this surf so that the per-package-dirs installation
dnl    goes to the correct directory without relying on the parent
dnl    bootstrap
AC_INIT(surf, 1.6-11, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sc10.tex])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET

STAR_LATEX_DOCUMENTATION(sc10) 
STAR_DOC_FILES(sc10_fig1.eps sc10_fig2.eps sc10_fig3.eps sc10_fig4.eps sc10_logo.eps)


dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
