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
AC_CONFIG_SRCDIR([sun216.tex])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET

STAR_LATEX_DOCUMENTATION(sun216) 
STAR_DOC_FILES(sun216_3c279.eps sun216_fitsdip.eps sun216_m82_remdbm.eps sun216_remsky.eps sun216_4chops.eps sun216_flow_jig.eps sun216_omc1vec.eps sun216_scansky.eps sun216_arrays.eps sun216_flow_photom.eps sun216_qdraw.eps sun216_shift_diag.eps sun216_despike_eg.eps sun216_flow_scan.eps sun216_rawsdip.eps sun216_skydip.eps sun216_despikemodes.eps sun216_logo.eps sun216_rawsdip2.eps )

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
