dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(ncar, 1.3-2, starlink@jiscmail.ac.uk)
 
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
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
AC_PROG_LN_S

dnl   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

dnl dependances on starlink packages
STAR_DECLARE_DEPENDENCIES(build, [gks])

dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Need this to allow gfortran to overflow in the BOZ assignments.
STAR_PRM_COMPATIBLE_SYMBOLS

dnl   Test what features the OPEN command can support
AC_FC_OPEN_SPECIFIERS(readonly)
AC_FC_OPEN_SPECIFIERS(action='read')

dnl   ISHIFT intrinsic
AC_FC_CHECK_INTRINSICS( [ishft ishift ishiftc ishftc] )

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
