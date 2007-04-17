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

if test $MERGED == yes ; then
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
AC_DEFINE(USE_COMPAT_CONST, 1, [For compatibility between tcl8.4 and previous tcl releases])

])

