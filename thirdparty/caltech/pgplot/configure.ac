dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    Note that for GKS the RAL version number if 1.37 but
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

common_built_drivers="exdriv.lo pgdriv.lo cadriv.lo hgdriv.lo lxdriv.lo ppdriv.lo vtdriv.lo gidriv.lo hidriv.lo psdriv.lo ttdriv.lo cwdriv.lo gldriv.lo nedriv.lo wddriv.lo hpdriv.lo lsdriv.lo nudriv.lo qmdriv.lo vadriv.lo cgdriv.lo nexsup.lo rvdriv.lo x2driv.lo xmdriv.lo figdisp_comm.lo pgxwin.lo tkdriv.lo xadriv.lo"

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

dnl    Stuff required for xathena
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
     (test "$have_xa3dlib" = "yes" || test "$have_xadlib" = "yes")])

dnl    xtk
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

dnl    xmotif
AC_ARG_WITH([motif],
             AC_HELP_STRING([--with-motif[=path]],
             [use XMotif (default location /usr/X11R6)]),
  [if test "$withval" = "yes"; then
    # Default place
    LIBS="$LIBS -L/usr/X11R6/lib"
    CPPFLAGS="$CPPFLAGS -I/usr/X11R6/include"
  elif test "$withval" = "no"; then
    : # We do nothing
  else
    XMLIBDIR=$withval/lib
    XMINCDIR=$withval/include
    LIBS="$LIBS -L$XMLIBDIR"
    CPPFLAGS="$CPPFLAGS -I$XMINCDIR"
  fi])

AC_CHECK_HEADER([Xm/XmP.h], set have_xmheader = "yes" ) 
AC_CHECK_LIB(Xm, xmp_PrependWidget, set have_xmlib = "yes" )
AM_CONDITIONAL( HAVE_XMOTIF, \
   [test "$have_xmheader" = "yes" && test "$have_xmlib" = "yes"] )

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [cnf gwm psx mers chr])
STAR_DECLARE_DEPENDENCIES([link],  [gwm cnf psx mers chr])


dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml cpg/Makefile fonts/Makefile pgdispd/Makefile src/Makefile sys/Makefile drivers/Makefile drivers/xathena/Makefile drivers/xtk/Makefile drivers/xmotif/Makefile])
AC_CONFIG_FILES(pgplot_link, [chmod +x pgplot_link])
AC_CONFIG_FILES(pgplot_link_adam, [chmod +x pgplot_link_adam])

dnl    This is the bit that does the actual work
AC_OUTPUT
