builtin(include,tclconfig/tcl.m4)
#  Need to pick up the proper version...
builtin(include,tclconfig/starconf.m4)

AC_DEFUN(GAIAVO_CONFIG, [

# Load the Tclutil definitions
cf=${prefix}/lib/tclutilConfig.sh
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
    AC_SUBST(CFITSIO_LIB_SPEC)
    AC_SUBST(CFITSIO_LIB_DIR)
else
    AC_MSG_ERROR([$cf doesn't exist])
fi

# Load the Astrotcl definitions
cf=${prefix}/lib/astrotclConfig.sh
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

# Load the Cat definitions
cf=${prefix}/lib/catConfig.sh
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

changequote(<<, >>)
csources=`cd ${srcdir}; echo generic/*.[Cc]`
changequote([, ])

gaia_headers=`cd ${srcdir}; echo generic/*.h generic/*.icc generic/*.hxx`
gaia_includes="-I. -I${srcdir}/generic -I${prefix}/include/cat -I${prefix}/include/astrotcl -I${prefix}/include/tclutil"
tclsources=`cd ${srcdir}; echo library/*.tcl library/*.wsdl library/*.xsl library/*.vot`

]) 
#  GAIAVO_CONFIG


AC_DEFUN(GAIAVO_COMPILER_TESTS, [

#  Tests that require the full compiler setup, including CFLAGS.

#  Some compiler flags are set by tcl.m4, but are protected to survive
#  unexpanded into Makefile, work around that (tcl.4m should not be handling
#  CFLAGS at all, it should stick to another name and merge that in the
#  Makefile, CFLAGS are for users, not systems).  
old_CFLAGS="$CFLAGS"
CFLAGS=`eval echo "$CFLAGS"`

#------------------------------------------------------------------------
#  Check if we require additional libraries to support C++ shareable
#  libraries. Solaris also has a broken bit_set to_string() function.
system=`uname -s`-`uname -r`
SHLIB_LD_CXX_LIBS=""
export SHLIB_LD_CXX_LIBS
case $system in
   SunOS-5*)
      SHLIB_LD_CXX_LIBS="-lCrun -lCstd"
      AC_DEFINE([HAVE_BROKEN_BIT_SET], 1, [Have Solaris bit_set broken headers])
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

dnl    Define support for large files.
STAR_DEFAULTS
STAR_LARGEFILE_SUPPORT

dnl    Need libtool for linking.
AC_CHECK_PROGS([LIBTOOL],[libtool])

PKG_LIBS="${PKG_LIBS} -lxerces-c `starX_link`"
SHLIB_PKG_LIBS="${PKG_LIBS}"
AC_SUBST(PKG_LIBS)
AC_SUBST(SHLIB_PKG_LIBS)

#  Restore full tcl.m4 CFLAGS.
CFLAGS="$old_CFLAGS"

])
#  GAIAVO_COMPILER_TESTS
