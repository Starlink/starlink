dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(photom, 1.9-0, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([figaro.news])

 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET

dnl dependances on starlink packages
STAR_DECLARE_DEPENDENCIES(build, [pda agi grp ndf fio])

dnl    We have a SUN document, we should probably tell people its there
STAR_LATEX_DOCUMENTATION(sun86)
STAR_DOC_FILES(sun86a.tex sun86_2df.eps   sun86_ech3.eps  sun86_hard.eps   sun86_imag.eps   sun86_spec.eps sun86_ech1.eps  sun86_ech4.eps  sun86_icont.eps  sun86_imag2.eps  sun86_spec2.eps sun86_ech2.eps  sun86_ech5.eps  sun86_igrey.eps  sun86_imag3.eps )

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    Need to go into sub directories
AC_CONFIG_SUBDIRS([applic appstl appsub fig fda dsa jty dta fit fla tio gen gks par ich appca appcb spext fitres work util sploop fibre graph main opt top twoutil unix])

dnl    This is the bit that does the actual work
AC_OUTPUT
