dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(pgplot-driver-xathena, 5.2.2-1, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([XaPgplot.c])



 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

AC_C_BIGENDIAN
AC_HEADER_STDC
AC_PATH_XTRA

AC_ARG_WITH([athena], 
             AC_HELP_STRING([--with-athena[=path]],
             [use Xaw (or Xaw3d) (default location /usr/X11R6/)]),
  [if test "$withval" = "yes"; then
    # Default place
    LIBS="$LIBS -L/usr/X11R6/lib"
    CPPFLAGS="$CPPFLAGS -I/usr/X11R6/include/X11"
  elif test "$withval" = "no"; then
    : # We do nothing
  else
    XALIBDIR=$withval/lib
    XAINCDIR=$withval/include
    LIBS="$LIBS -L$XALIBDIR"
    CPPFLAGS="$CPPFLAGS -I$XAINCDIR"
  fi])

dnl    Hack and slash for the Mac. This should only add goodness to 
dnl    non-Mac builds in the case where whoever did their filesystem 
dnl    have done something as wierd as Apple.
LIBS="$LIBS -L/usr/X11R6/lib"
CPPFLAGS="$CPPFLAGS -I/usr/X11R6/include"

AC_CHECK_HEADER([X11/Xaw3d/Simple.h], set have_xa3dheader = "yes" ) 
AC_CHECK_HEADER([X11/Xaw/Simple.h], set have_xaheader = "yes" ) 

AC_CHECK_LIB(Xaw3d, XawInitializeWidgetSet, set have_xa3dlib = "yes" )
AC_CHECK_LIB(Xaw, XawInitializeWidgetSet, set have_xalib = "yes" )


AM_CONDITIONAL( HAVE_XATHENA, \
   [(test "$have_xa3dheader" = "yes" || test "$have_xaheader" = "yes") && \
    (test "$test_xa3dlib" = "yes" || test "$test_xadlib" = "yes")] )

     
dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS


dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
