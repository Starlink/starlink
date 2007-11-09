dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    Note that for GKS the RAL version number is 1.37 but
dnl    Starlink does not increment that version. Rather than use
dnl    non-standard nomenclature use the patch level for starlink patching.
AC_INIT(pgplot, 5.2.2-2, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([dummy_fortran.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL
AC_C_BIGENDIAN
AC_HEADER_STDC
AC_PATH_XTRA
AC_CHECK_HEADERS(values.h)

dnl    Other options here, such as
AC_FC_LIBRARY_LDFLAGS
STAR_CNF_COMPATIBLE_SYMBOLS

dnl   These are the driver files that will actually be built.
dnl   In true autoconf style, we do not attempt to generate this list
dnl   automatically (see the automake manual's FAQ for justification).
dnl
dnl   The common_built_drivers are common to the standard set and the
dnl   Starlink set.

common_built_drivers="exdriv.lo pgdriv.lo cadriv.lo hgdriv.lo lxdriv.lo ppdriv.lo vtdriv.lo gidriv.lo hidriv.lo psdriv.lo ttdriv.lo cwdriv.lo gldriv.lo nedriv.lo wddriv.lo hpdriv.lo lsdriv.lo nudriv.lo qmdriv.lo vadriv.lo cgdriv.lo nexsup.lo rvdriv.lo x2driv.lo xmdriv.lo figdisp_comm.lo pgxwin.lo tkdriv.lo pkdriv.lo xadriv.lo"

dnl   Add the png driver if the PNG header and library are available.
AC_CHECK_HEADER(png.h,
                AC_CHECK_LIB(png, png_info_init,
                             [LIBS="$LIBS -lpng";BUILT_DRIVERS="$BUILT_DRIVERS pndriv.lo"],
                             [AC_MSG_WARN(PNG libraries not found)],
                             [-lz]))

BUILT_DRIVERS="$common_built_drivers xwdriv.lo"
STAR_BUILT_DRIVERS="$common_built_drivers star_xwdriv.lo gwmdriv.lo"

AC_SUBST(BUILT_DRIVERS)
AC_SUBST(STAR_BUILT_DRIVERS)

# See if we have X
AC_PATH_XTRA

dnl    Stuff required for xathena
have_athena="yes"
AC_ARG_WITH([athena], 
             AC_HELP_STRING([--with-athena[=path]],
             [use Xaw3d (default location /usr/X11R6/)]),
  [if test "$withval" = "yes"; then
    # Default place
    LIBS="$LIBS $X_LIBS"
    CPPFLAGS="$CPPFLAGS $X_CFLAGS"
  elif test "$withval" = "no"; then
    # We do nothing
    have_athena="no"
  else
    XALIBDIR=$withval/lib
    XAINCDIR=$withval/include
    LIBS="$LIBS -L$XALIBDIR"
    CPPFLAGS="$CPPFLAGS -I$XAINCDIR"
  fi])

dnl    Hack and slash for the Mac. This should only add goodness to 
dnl    non-Mac builds in the case where whoever did their filesystem 
dnl    have done something as wierd as Apple.
LIBS="$LIBS $X_LIBS"
CPPFLAGS="$CPPFLAGS $X_CFLAGS"
if test "$have_athena" = "yes"; then
  AC_CHECK_HEADER([X11/Xaw3d/Simple.h], have_xa3dheader=yes ) 
  AC_CHECK_LIB(Xaw3d, XawInitializeWidgetSet, have_xa3dlib=yes )
else
  have_xa3dheader="no"
  have_xa3dlib="no"
fi

AM_CONDITIONAL( HAVE_XATHENA, \
    [test "$have_xa3dheader" = "yes" && test "$have_xa3dlib" = "yes" ])

dnl    xtk & ptk, use the Starlink version of Tcl/Tk, so don't need to check
dnl    for it (declared as a build dependency). To keep the various 
dnl    sub-components happy we need to define HAVE_TK as a conditional.
dnl    This will always be true.
STAR_PATH_TCLTK(8.4,[tk])
AM_CONDITIONAL( HAVE_TK, : )

dnl    xmotif
have_motif="yes"
AC_ARG_WITH([motif],
             AC_HELP_STRING([--with-motif[=path]],
             [use XMotif (default location /usr/X11R6)]),
  [if test "$withval" = "yes"; then
    # Default place
    LIBS="$LIBS $X_LIBS"
    CPPFLAGS="$CPPFLAGS $X_CFLAGS"
  elif test "$withval" = "no"; then
    # We do nothing
    have_motif="no"
  else
    XMLIBDIR=$withval/lib
    XMINCDIR=$withval/include
    LIBS="$LIBS -L$XMLIBDIR"
    CPPFLAGS="$CPPFLAGS -I$XMINCDIR"
  fi])

if test "$have_motif" = "yes"; then
   AC_CHECK_HEADER([Xm/XmP.h], have_xmheader=yes ) 
   AC_CHECK_LIB(Xm, XmScrolledWindowSetAreas, have_xmlib=yes )
else
   have_xmheader="no"
   have_xmlib="no"
fi

AM_CONDITIONAL( HAVE_XMOTIF, \
   [test "$have_xmheader" = "yes" && test "$have_xmlib" = "yes"] )

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [cnf gwm psx mers chr tcl tk])
STAR_DECLARE_DEPENDENCIES([link],  [gwm cnf psx mers chr tcl tk])


dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml cpg/Makefile fonts/Makefile pgdispd/Makefile src/Makefile sys/Makefile examples/Makefile drivers/Makefile drivers/xathena/Makefile drivers/xtk/Makefile drivers/ptk/Makefile drivers/xmotif/Makefile])
AC_CONFIG_FILES(pgplot_link, [chmod +x pgplot_link])
AC_CONFIG_FILES(pgplot_link_adam, [chmod +x pgplot_link_adam])

dnl    This is the bit that does the actual work
AC_OUTPUT
