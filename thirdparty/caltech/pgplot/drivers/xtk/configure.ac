dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(pgplot-driver-xtk, 5.2.2-1, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([tkpgplot.c])

 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

AC_HEADER_STDC
AC_PATH_XTRA

AC_ARG_WITH([tk], 
             AC_HELP_STRING([--with-tk[=path]],
             [use Tk (default location /usr)]),
  [if test "$withval" = "yes"; then
    # Default place, installed by Fink in /usr
    LIBS="$LIBS -L/usr/lib"
    CPPFLAGS="$CPPFLAGS -I/usr/include"
  elif test "$withval" = "no"; then
    : # We do nothing
  else
    TKLIBDIR=$withval/lib
    TKINCDIR=$withval/include
    LIBS="$LIBS -L$TKLIBDIR"
    CPPFLAGS="$CPPFLAGS -I$TKINCDIR"
  fi])

AC_CHECK_HEADER([tk.h], set have_tkheader = "yes" ) 
AC_CHECK_LIB(tk, Tk_DisplayName, set have_tklib = "yes" )
AM_CONDITIONAL( HAVE_TK, \
   [test "$have_tkheader" = "yes" && test "$have_tklib" = "yes"] )
     
dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS


dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
