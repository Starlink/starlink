dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(datacube, 1.0-4, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([datacube_mon.f])

dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS(per-package-dirs)
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_MONOLITHS

STAR_CHECK_PROGS([hlib fgeneric])

dnl    Conditional compile for missing symbols on Mac OSX. Using this in
dnl    an odd way, essentially to check whether we're on a Mac. If we are
dnl    we need to link against this library, it will be included by default
dnl    if it exists in this case.
dnl
dnl    XXX This is very probably redundant.  This check is also now done
dnl    within STAR_DEFAULTS
AC_CHECK_LIB(cc_dynamic, restFP)

dnl    Dependences on starlink packages: depends on kappa, figaro and
dnl    convert for the scripts in the package to work, so these must
dnl    be declared as `use' dependencies.
STAR_DECLARE_DEPENDENCIES(build, [generic ndf par sae shl mers pcs], [link])
STAR_DECLARE_DEPENDENCIES(use,   [kappa figaro convert])

dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS

STAR_LATEX_DOCUMENTATION(sun237/ sc16/)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml datacube.csh datacube.icl
    help/Makefile subs/Makefile tasks/Makefile
    sun237/Makefile sc16/Makefile
    demo/Makefile demo/datacube_demo.csh])

dnl    This is the bit that does the actual work
AC_OUTPUT
