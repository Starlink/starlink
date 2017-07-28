dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number.
AC_INIT([dipso],[3.6-5],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])
 
dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([dipso.f])
 
dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES(build, [ast cat kaplibs snx pda ndf sae perl], link)
STAR_DECLARE_DEPENDENCIES(sourceset, star2html)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
LT_INIT

dnl  Need a main and typeless Hex constants
AC_FC_MAIN
AC_FC_HAVE_BOZ
AC_FC_HAVE_OLD_TYPELESS_BOZ

STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS
AC_FC_LIBRARY_LDFLAGS

dnl    Do some Sun checks
AC_CHECK_HEADERS([floatingpoint.h])
AC_CHECK_FUNCS([ieee_handler])

dnl    Documentation.
STAR_LATEX_DOCUMENTATION(sun50)

dnl    Pick the best awk command, the _link script is quite demanding
AC_PROG_AWK

STAR_CHECK_PROGS(perl)

AC_CONFIG_HEADERS(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES([my_dipso], [chmod +x my_dipso])
AC_CONFIG_FILES([dipso_link], [chmod +x dipso_link])

dnl    This is the bit that does the actual work
AC_OUTPUT
