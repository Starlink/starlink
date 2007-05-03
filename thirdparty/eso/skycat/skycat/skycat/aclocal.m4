builtin(include,../tclconfig/tcl.m4)

AC_DEFUN(SKYCAT_CONFIG, [

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
    AC_SUBST(BLT_LIB_DIR)
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

# Load the Rtd definitions
cf=../rtd/rtdConfig.sh
if test -f $cf ; then
    . $cf
    AC_SUBST(rtd_VERSION)
    AC_SUBST(rtd_LIB_FILE)
    AC_SUBST(rtd_BUILD_LIB_SPEC)
    AC_SUBST(rtd_BUILD_DIR)
    AC_SUBST(rtd_LIB_SPEC)
    AC_SUBST(rtd_SRC_DIR)
    AC_SUBST(rtd_PKG_OBJECTS)
else
    AC_MSG_ERROR([$cf doesn't exist])
fi

# Load the Cat definitions
cf=../cat/catConfig.sh
if test -f $cf ; then
    . $cf
    AC_SUBST(cat_VERSION)
    AC_SUBST(cat_LIB_FILE)
    AC_SUBST(cat_BUILD_LIB_SPEC)
    AC_SUBST(cat_BUILD_DIR)
    AC_SUBST(cat_LIB_SPEC)
    AC_SUBST(cat_SRC_DIR)
    AC_SUBST(rtd_PKG_OBJECTS)
else
    AC_MSG_ERROR([$cf doesn't exist])
fi

# -----------------------------------------------------------------------
# Optionally merge object and header files from dependent packages to make one master rtd lib
AC_ARG_ENABLE(merge, 
    [AC_HELP_STRING([--enable-merge],[merge the contents of dependent packages into a master library])],
    [MERGED=$enableval],
    [MERGED=no])

changequote(<<, >>)
csources=`cd $srcdir; echo generic/*.[Cc]`
changequote([, ])

# For skycat, we merge in the cat, rtd, astrotcl and tclutil libs, to avoid the extra dependencies
skycat_headers=`cd $srcdir; echo generic/*.h`
cat_headers=`cd $srcdir; echo ../cat/generic/*.h`
rtd_headers=`cd $srcdir; echo ../rtd/generic/*.h ../rtd/rtdevt/rtdImageEvent.h ../rtd/rtdevt/rtdSem.h`
astrotcl_headers=`cd $srcdir; echo ../astrotcl/{generic,press,libwcs,cfitsio}/*.h`
tclutil_headers=`cd $srcdir; echo ../tclutil/generic/*.h`

skycat_includes="-I$srcdir/generic -I$srcdir/bitmaps"
cat_includes="-I$srcdir/../cat/generic"
rtd_includes="-I$srcdir/../rtd/generic -I$srcdir/../rtd/rtdevt"
astrotcl_includes="-I$srcdir/../astrotcl/generic -I$srcdir/../astrotcl/cfitsio -I$srcdir/../astrotcl/libwcs"
tclutil_includes="-I$srcdir/../tclutil/generic"
cincludes="${skycat_includes} ${cat_includes} ${rtd_includes} ${astrotcl_includes} ${tclutil_includes}"

tclsources=`cd $srcdir; echo library/*.tcl library/*.xpm`

if test $MERGED = "yes" ; then
    echo "Will build merged master skycat library"
    cheaders="${skycat_headers} ${cat_headers} ${rtd_headers} ${astrotcl_headers} ${tclutil_headers}"
    MERGE_OBJECTS="$cat_PKG_OBJECTS $rtd_PKG_OBJECTS $astrotcl_PKG_OBJECTS $tclutil_PKG_OBJECTS"
else
    echo "Not making a merged master skycat library"
    cheaders="${skycat_headers}"
    MERGE_OBJECTS=""
fi
AC_SUBST(MERGE_OBJECTS)

# -----------------------------------------------------------------------
# 	Check if we need (or can use) the socklen_t type.
AC_CHECK_TYPES([socklen_t],,,[#include <sys/socket.h>])

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
      if test "$CXX" = "cxx"; then 
         CXXFLAGS="$CXXFLAGS -g3 -std gnu -D__USE_STD_IOSTREAM=1"
      fi
  ;;
esac


])

