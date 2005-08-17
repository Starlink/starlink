dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(ncar, 1.3-1, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([ncar.h])
 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
dnl    Note we disable shared libraries by default as these cause problems
dnl    with SNX which shares the common-blocks.
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_DISABLE_SHARED
AC_PROG_LIBTOOL
AC_PROG_LN_S

dnl dependances on starlink packages
STAR_DECLARE_DEPENDENCIES(build, [gks])

dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS

dnl   Test what features the OPEN command can support
AC_FC_OPEN_SPECIFIERS(readonly)

dnl   What kind of hexadecimal constants are available.
AC_FC_HAVE_BOZ
AC_FC_HAVE_OLD_TYPELESS_BOZ

dnl    documentation
STAR_LATEX_DOCUMENTATION(sun88)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.

dnl    We recurse into subdirectories -- see SUBDIRS in Makefile.am
AC_CONFIG_FILES([Makefile component.xml
    data/Makefile examples/Makefile])

dnl    This is the bit that does the actual work
AC_OUTPUT
