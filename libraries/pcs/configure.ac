dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Configure file for the PCS system.  This is just a shell for
dnl    the various subsystems, so this configure file is atypical.  It
dnl    is not at present intended to be distributable by itself.  Its
dnl    main function is to express the dependence of this package on
dnl    its subpackages, and to manage the PCS docs.

dnl    Initialisation: package name and version number
AC_INIT(pcs, 4.1-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([ssn29.tex])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Declare the build and use dependencies for this package
dnl    The dependencies should expand to include each of the
dnl    subpackage as they are added to the system
STAR_DECLARE_BUILD_DEPENDENCIES(subpar)
dnl STAR_DECLARE_USE_DEPENDENCIES()

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(ssn29 ssn77 sun115 sun134 sun144 sun224 sun241)

dnl    List the set of active subdirectories.
dnl    This must be kept up-to-date by hand.
AC_CONFIG_SUBDIRS(adam adamnet ams dtask hdspar lex messys misc msp parsecon sock string subpar task)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
