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
AC_CONFIG_SRCDIR([sc11.tex])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET

STAR_LATEX_DOCUMENTATION(sc11) 
STAR_DOC_FILES(sc11_fig1.eps   sc11_fig12.eps  sc11_fig4.eps  sc11_fig7.eps  sc11_flow.eps sc11_fig10.eps  sc11_fig2.eps   sc11_fig5.eps  sc11_fig8.eps  sc11_logo.eps sc11_fig11.eps  sc11_fig3.eps   sc11_fig6.eps  sc11_fig9.eps)



dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
