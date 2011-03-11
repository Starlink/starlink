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
gaia_includes="-I. -I${srcdir}/generic -I${srcdir}/bitmaps -I${prefix}/include/skycat -I${prefix}/include/rtd -I${prefix}/include/cat -I${prefix}/include/astrotcl -I${prefix}/include/tclutil"
tclsources=`cd ${srcdir}; echo library/*.tcl library/*.itk library/*.xpm library/skycat2.0.cfg`

#  Size of long, used for FITS.
AC_CHECK_SIZEOF(long,4)

]) 
#  GAIA_CONFIG


AC_DEFUN(GAIA_COMPILER_TESTS, [

#  Tests that require the full compiler setup, including CFLAGS.

#  Some compiler flags are set by tcl.m4, but are protected to survive
#  unexpanded into Makefile, work around that (tcl.4m should not be handling
#  CFLAGS at all, it should stick to another name and merge that in the
#  Makefile, CFLAGS are for users, not systems).  
old_CFLAGS="$CFLAGS"
CFLAGS=`eval echo "$CFLAGS"`

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
      SHLIB_LD_CXX_LIBS="-lCrun -lCstd"
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

#  Fortran support. Also need Starlink installation and FCFLAGS, so 
#  have included "starconf.m4" (see top).
STAR_DEFAULTS
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS
AC_PROG_FC
AC_CHECK_PROGS(LIBTOOL,libtool)
STAR_CHECK_PROGS([fgeneric])
STAR_INITIALISE_FORTRAN

dnl Pre-C99 compilers do not support inline, check for that.
AC_C_INLINE

dnl    Sometimes also need to know the name of the Fortran MAIN.
AC_FC_MAIN

#--------------------------------------------------------------------
#  Try to determine the extra libraries required to link a
#  C/C++ program that has f77 libraries or subroutines.
#--------------------------------------------------------------------
#  Protect this against LIBS values, which are C specific (from Tcl).
#  This is a general problem with testing Fortran properties, but
#  only effects this macro.
old_LIBS=$LIBS
LIBS=
AC_FC_LIBRARY_LDFLAGS
LIBS=$old_LIBS

#  Hack for intel darwin g95, includes libgcc_eh.a, but that causes symbols
#  to clash with the official compiler.
FCLIBS="`echo $FCLIBS | sed 's:-lgcc_eh::g'`"

#  Solaris 8 includes libcx, which isn't required and causes issues with 
#  shareable libraries (when not linked with FC). Strip
#  that out.        
FCLIBS="`echo $FCLIBS | sed 's:-lcx::g'`"

STAR_LARGEFILE_SUPPORT

#  GAIA needs several Starlink libraries for linking. Use -latl instead
#  of atl_link to avoid picking up two versions of cfitsio (skycat and
#  starlink). 
PKG_LIBS="${PKG_LIBS} ${STAR_LDFLAGS} -latl `ard_link -myerr -grf_v5.6 -grf3d` \
`fio_link` -L${STARLINK}/lib/startcl -ltclAdam `ams_link_adam` \
`ndf_link -myerr -grf_v5.6 -grf3d` -lpda `ast_link -myerr -grf3d` `sla_link`"

#  On Alphas we get unwanted flags to control the address space. 
#  Remove these when building shareable library.
SHLIB_PKG_LIBS="`echo $PKG_LIBS| sed 's:-Wl,-D -Wl,40800000 -Wl,-T -Wl,30000000::g'`"
AC_SUBST(SHLIB_PKG_LIBS)

#  Restore full tcl.m4 CFLAGS.
CFLAGS="$old_CFLAGS"

])
#  GAIA_COMPILER_TESTS
