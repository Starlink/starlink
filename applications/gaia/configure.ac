dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(gaia, 2.8-2, gaia@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([gaia.news])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS(per-package-dirs)

dnl    Need to build an atask, depends on much the same as a full monolith.
STAR_MONOLITHS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

dnl    We have a C++ main function, but we call Fortran.  This defines
dnl    the cpp macro of the same name, which will do any 
dnl    initialisation which is necessary with this platform/compiler.
STAR_INITIALISE_FORTRAN

dnl    Declare dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, 
                          [ard ast cat fio ndf pcs pda sae], link)
STAR_DECLARE_DEPENDENCIES(sourceset, star2html)

dnl    Need to add AUTOASTROM to this when available.
STAR_DECLARE_DEPENDENCIES(use, [convert esp extractor kappa photom polpack])


dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun214)

AC_CONFIG_HEADERS(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
