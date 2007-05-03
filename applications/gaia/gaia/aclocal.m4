builtin(include,tclconfig/tcl.m4)
#  Need to pick up the proper version...
builtin(include,tclconfig/starconf.m4)

AC_DEFUN(GAIA_CONFIG, [

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

# Load the Rtd definitions
cf=${prefix}/lib/rtdConfig.sh
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

# Load the SkyCat definitions
cf=${prefix}/lib/skycatConfig.sh
if test -f $cf ; then
    . $cf
    AC_SUBST(skycat_VERSION)
    AC_SUBST(skycat_LIB_FILE)
    AC_SUBST(skycat_BUILD_LIB_SPEC)
    AC_SUBST(skycat_BUILD_DIR)
    AC_SUBST(skycat_LIB_SPEC)
    AC_SUBST(skycat_SRC_DIR)
else
    AC_MSG_ERROR([$cf doesn't exist])
fi

changequote(<<, >>)
csources=`cd ${srcdir}; echo generic/*.[Cc] tkhtml/*.c`
fsources=`cd ${srcdir}; echo generic/*.f`
gsources=`cd ${srcdir}; echo generic/*.gen generic/*.gsc generic/*.gsn`
changequote([, ])

gaia_headers=`cd ${srcdir}; echo generic/*.h generic/*.icc`
gaia_includes="-I${srcdir}/generic -I${srcdir}/bitmaps -I${prefix}/include/skycat -I${prefix}/include/rtd -I${prefix}/include/cat -I${prefix}/include/astrotcl -I${prefix}/include/tclutil"
tclsources=`cd ${srcdir}; echo library/*.tcl library/*.itk library/*.xpm library/skycat2.0.cfg`

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
case $system in
   OSF*) 
      if test "$CXX" = "cxx"; then 
         CXXFLAGS="$CXXFLAGS -g3 -std gnu -D__USE_STD_IOSTREAM=1"
      fi
  ;;
esac

# Fortran support. Also need Starlink installation and FCFLAGS, so 
# have included "starconf.m4" (see top).
STAR_DEFAULTS
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS
AC_PROG_FC
STAR_CHECK_PROGS([fgeneric])

STAR_LARGEFILE_SUPPORT

# GAIA needs several Starlink libraries for linking.
PKG_LIBS="${PKG_LIBS} ${STAR_LDFLAGS} `atl_link` `ard_link -myerr -mygrf` \
`fio_link` -L${STARLINK}/lib/startcl -ltclAdam `ams_link_adam` \
`ndf_link -myerr -mygrf` -lpda `ast_link -myerr` `sla_link`"

])
