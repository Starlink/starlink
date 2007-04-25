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

])

