builtin(include,../tclconfig/tcl.m4)

AC_DEFUN(CAT_CONFIG, [

# Load the Tclutil definitions
cf=../tclutil/tclutilConfig.sh
if test -f $cf ; then
    . $cf
    AC_SUBST(tclutil_VERSION)
    AC_SUBST(tclutil_LIB_FILE)
    AC_SUBST(tclutil_BUILD_LIB_SPEC)
    AC_SUBST(tclutil_BUILD_DIR)
    AC_SUBST(tclutil_LIB_SPEC)
    AC_SUBST(BLT_LIB_SPEC)
    AC_SUBST(tclutil_SRC_DIR)
    AC_SUBST(tclutil_PKG_OBJECTS)
else
    AC_MSG_ERROR([$cf doesn't exist])
fi

# Load the Astrotcl definitions
cf=../astrotcl/astrotclConfig.sh
if test -f $cf ; then
    . $cf
    AC_SUBST(astrotcl_VERSION)
    AC_SUBST(astrotcl_LIB_FILE)
    AC_SUBST(astrotcl_BUILD_LIB_SPEC)
    AC_SUBST(astrotcl_BUILD_DIR)
    AC_SUBST(astrotcl_LIB_SPEC)
    AC_SUBST(astrotcl_SRC_DIR)
    AC_SUBST(astrotcl_PKG_OBJECTS)
else
    AC_MSG_ERROR([$cf doesn't exist])
fi

# -----------------------------------------------------------------------
# Optionally merge object and header files from dependent packages to make one master lib
# -----------------------------------------------------------------------
MERGED=1
AC_ARG_ENABLE(merge, 
    [AC_HELP_STRING([--enable-merge],[merge the contents of dependent packages into a master catalog library])],
    [MERGED=$enableval],
    [MERGED=no])

changequote(<<, >>)
csources=`cd $srcdir; echo generic/*.[Cc]`
changequote([, ])

cat_headers=`cd $srcdir; echo generic/*.h`
astrotcl_headers=`cd $srcdir; echo ../astrotcl/{generic,press,libwcs,cfitsio}/*.h`
tclutil_headers=`cd $srcdir; echo ../tclutil/generic/*.h`

cat_includes="-I$srcdir/generic -I$srcdir/bitmaps"
astrotcl_includes="-I$srcdir/../astrotcl/generic -I$srcdir/../astrotcl/cfitsio -I$srcdir/../astrotcl/libwcs"
tclutil_includes="-I$srcdir/../tclutil/generic"
cincludes="${cat_includes} ${astrotcl_includes} ${tclutil_includes}"

tclsources=`cd $srcdir; echo library/*.tcl library/*.xpm`

if test $MERGED = yes ; then
    echo "Will build merged master catalog library"
    cheaders="${cat_headers} ${astrotcl_headers} ${tclutil_headers}"
    MERGE_OBJECTS="$astrotcl_PKG_OBJECTS $tclutil_PKG_OBJECTS"
dnl    AC_DEFINE(MERGE_OBJECTS, 1, [merge the contents of dependent packages into a master catalog library])
else
    echo "Not making a merged master catalog library"
    cheaders="${cat_headers}"
    MERGE_OBJECTS=""
fi
AC_SUBST(MERGE_OBJECTS)

# -----------------------------------------------------------------------
AC_DEFINE(USE_COMPAT_CONST, 1, [For compatibility between tcl8.4 and previous tcl releases])

#------------------------------------------------------------------------
#  Check if we require additional libraries to support C++ shareable
#  libraries.
system=`uname -s`-`uname -r`
SHLIB_LD_CXX_LIBS=""
export SHLIB_LD_CXX_LIBS
case $system in
   SunOS-5*)
      SHLIB_LD_CXX_LIBS="/usr/lib/libCrun.so.1 /usr/lib/libCstd.so.1"
   ;;
   OSF*)
      SHLIB_LD_CXX_LIBS="-lcxx -lcxxstd"
   ;;
esac
AC_SUBST(SHLIB_LD_CXX_LIBS)

#-------------------------------------------------------------------------
#  The cxx C++ compiler under Tru64 UNIX needs the special
#  CXXFLAGS "-std gnu -D__USE_STD_IOSTREAM=1". These allow the standard 
#  library streams headers to work and to generate templates that do 
#  not require special handling throughout skycat directories (normally 
#  template object files are created in various cxx_repository subdirectories,
#  this way the object files are kept embedded the usual object files, see 
#  the cxx man page for details).
#-------------------------------------------------------------------------
export CXXFLAGS
case $system in
   OSF*) 
      case "x$CXX" in
         xcxx*)
            CXXFLAGS="$CXXFLAGS -g3 -std gnu -D__USE_STD_IOSTREAM=1"
         ;;
      esac
  ;;
esac
])

