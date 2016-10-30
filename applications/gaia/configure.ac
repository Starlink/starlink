dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    *** After changing this value make sure that bootstrap is
dnl    run and gaia/configure, gaia3d/configure and gaiavo/configure 
dnl    scripts have been updated (this normally requires a bootstrap, 
dnl    configure, bootstrap, the configure updates the local directories 
dnl    which then require regenerating again).
AC_INIT([gaia],[4.4-8],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE([1.8.2-starlink])

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([gaia.news])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS([per-package-dirs])

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC
LT_INIT

dnl    Need to build an atask, depends on much the same as a full monolith.
STAR_MONOLITHS

dnl    We have a C++ main function, but we call Fortran.  This defines
dnl    the cpp macro of the same name, which will do any 
dnl    initialisation which is necessary with this platform/compiler.
STAR_INITIALISE_FORTRAN_RTL

dnl    Sometimes also need to know the name of the Fortran MAIN.
AC_FC_MAIN

dnl    Need the generic compiler.
STAR_CHECK_PROGS([fgeneric])

dnl    Look for a Tcl installation. Prefer the Starlink one as we require
dnl    itcl, tclx and BLT, but they are not supported by this command.
STAR_PATH_TCLTK(8.5, [tk])

dnl    Define support for large files.
STAR_LARGEFILE_SUPPORT

dnl    Declare dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [ard atl ast blt cat cfitsio generic dnl
fio gsl img itcl pcs pda sae skycat startcl sla tcl tclx tk tcllibs vtk dnl
xerces-c tdom], link)
STAR_DECLARE_DEPENDENCIES(sourceset, star2html)
STAR_DECLARE_DEPENDENCIES(configure, [tcl tk blt itcl])

dnl    Need to add AUTOASTROM to this when available.
STAR_DECLARE_DEPENDENCIES(use, [convert esp extractor kappa photom dnl
polpack startcl tcl tk blt tclx tcllibs skycat tkimg])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun214)

dnl    Setup headers for defined macros.
AC_CONFIG_HEADERS(config.h)

dnl    To configure the gaia component, which is modelled on the skycat
dnl    TEA-based Tcl system we would like to just run ./configure in the gaia
dnl    subdirectory, but it has some configure dependencies that may not
dnl    be available yet, so it's deferred until make time (see teagaia).
CONFIGURE_ARGS="$ac_configure_args"
export CONFIGURE_ARGS
AC_SUBST(CONFIGURE_ARGS)

dnl    Make sure configure is ran in sub-directories.
test -f "gaia/Makefile" && rm -f "gaia/Makefile"
test -f "gaia3d/Makefile" && rm -f "gaia3d/Makefile"
test -f "gaiavo/Makefile" && rm -f "gaiavo/Makefile"

dnl    Need to determine the suffix for shareable libraries, as used by
dnl    Tcl, so we can link against the GAIA dynamic library.
tcl_system=`uname -s`-`uname -r`
case $tcl_system in
   windows)
      TCL_SHLIB_EXT='.dll'
      ;;
   Darwin-*)
      TCL_SHLIB_EXT='.dylib'
      ;;
   *)
      TCL_SHLIB_EXT='.so'
      ;;
esac
AC_SUBST(TCL_SHLIB_EXT)

dnl    The PACKAGE_VERSION values are slightly different for TCL and 
dnl    starconf, make sure the TCL version is available.
TCL_PACKAGE_VERSION="`echo $PACKAGE_VERSION | sed 's:\-:\.:'`"
export TCL_PACKAGE_VERSION
AC_SUBST(TCL_PACKAGE_VERSION)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile tasks/Makefile scripts/Makefile teagaia/Makefile])
AC_CONFIG_FILES([component.xml])
AC_CONFIG_FILES([gaia/configure.in gaia3d/configure.in gaiavo/configure.in])

dnl    This is the bit that does the actual work
AC_OUTPUT
