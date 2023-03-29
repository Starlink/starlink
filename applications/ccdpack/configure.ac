AC_INIT([ccdpack],[4.1-1],[ccdpack@starlink.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR(ccdexercise)
STAR_DEFAULTS(per-package-dirs)
AC_PROG_CC
AC_PROG_FC
STAR_MONOLITHS
LT_INIT
STAR_CNF_COMPATIBLE_SYMBOLS

STAR_CHECK_PROGS([hlib ifd2star fgeneric])
AC_FC_LIBRARY_LDFLAGS
AC_FC_MAIN

STAR_MESSGEN(user_err.msg)

TCL_VERSION=8.6
ITCL_VERSION=4.2.3
ITK_VERSION=4.1.0
IWIDGETS_VERSION=4.1.1
BLT_VERSION=25

AC_PATH_XTRA

AC_SUBST(TCL_VERSION)
AC_SUBST(ITCL_VERSION)
AC_SUBST(IWIDGETS_VERSION)
AC_SUBST(BLT_VERSION)

# Define CCDPACK_DIR as a macro for baking into ccdwish.
CCDPACK_DIR=\"${prefix}/bin/ccdpack\"
AC_DEFINE_UNQUOTED(CCDPACK_DIR, $CCDPACK_DIR, [Compile time CCDPACK_DIR])

LD_LIBRARY_PATH="${STARLINK}/lib:${LD_LIBRARY_PATH}"
export LD_LIBRARY_PATH

dnl   Check for fpu_control.h, this isn't available for all  
dnl   GCC implementations 
AC_CHECK_HEADERS([fpu_control.h])

# Check availability of Tcl/Tk and friends.  Note that the Starlink Tcl, Tk,
# ITcl and BLT are required, since we need some somewhat-private include
# files. If no Starlink versions are located, we do without ITcl and BLT.
STAR_PATH_TCLTK($TCL_VERSION, [tk])
if test "${TCL_PREFIX}" != ""; then
   my_save_LDFLAGS=$LDFLAGS
   LDFLAGS="$STAR_LDFLAGS $LDFLAGS"
   have_itcl=1
   my_save_LIBS="$LIBS"
   LIBS="$LIBS $X_LIBS $X_EXTRA_LIBS $X_PRE_LIBS -lX11"
   AC_CHECK_LIB(itcl$ITCL_VERSION,Itcl_Init,,[unset have_itcl],[$TCL_LIBS])
   AC_CHECK_LIB(itk$ITK_VERSION,Itk_Init,,[unset have_itcl],[-litcl$ITCL_VERSION -litclstub$ITCL_VERSION $TK_LIBS $TCL_LIBS])
   AC_MSG_CHECKING([whether to use incr tcl])
   if test -n "$have_itcl"; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_ITCL, 1, [Is incr Tcl present?])
      ITCL_LIBS="-litk$ITK_VERSION -litcl$ITCL_VERSION"
      AC_SUBST(ITCL_LIBS)
   else
      AC_MSG_RESULT(no)
   fi
   LIBS="$my_save_LIBS"

   have_blt=1
   my_save_LIBS="$LIBS"
   LIBS="$LIBS $X_LIBS $X_EXTRA_LIBS $X_PRE_LIBS -lX11"
   AC_CHECK_LIB(BLT$BLT_VERSION,Blt_Init,,[unset have_blt],[-lBLT$BLT_VERSION -lBLTlite$BLT_VERSION $TK_LIBS $TCL_LIBS])
   AC_MSG_CHECKING([whether to use BLT])
   if test -n "$have_blt"; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_BLT, 1, [Is BLT present?])
      BLT_LIBS="-lBLT$BLT_VERSION -lBLTlite$BLT_VERSION"
      AC_SUBST(BLT_LIBS)
   else
      AC_MSG_RESULT(no)
   fi
   LIBS="$my_save_LIBS"

   LDFLAGS=$my_save_LDFLAGS
fi

# Dependencies must be on one line
STAR_DECLARE_DEPENDENCIES([build], [ard ast blt chr cnf fio generic grp hds idi ifd itcl ndf ndg one par pcs pgplot prm psx tcl tk trn])
STAR_DECLARE_DEPENDENCIES([build], [agi blt graphpar img pda ref shl sla startcl],[link])
STAR_DECLARE_DEPENDENCIES(configure, [itcl tcl tk blt])

STAR_LATEX_DOCUMENTATION([sun139])

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile component.xml ccdpack.ifd CCD1_PAR
    gen/Makefile  help/Makefile  main/Makefile  tasks/Makefile])
AC_CONFIG_FILES([gen/dgeneric], [chmod +x gen/dgeneric])

AC_OUTPUT
