builtin(include,tclconfig/tcl.m4)
#  Need to pick up the proper version...
builtin(include,tclconfig/starconf.m4)

AC_DEFUN(GAIA3D_CONFIG, [

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
sources=`cd ${srcdir}; echo generic/*.[Cc]`
changequote([, ])

headers=`cd ${srcdir}; echo generic/*.h`
includes="-I. -I${srcdir}/generic -I../gaia/generic -I${prefix}/include/skycat \
-I${prefix}/include/rtd -I${prefix}/include/cat -I${prefix}/include/astrotcl \
-I${prefix}/include/tclutil -I${prefix}/include/vtk"
tclsources=`cd ${srcdir}; echo library/*.tcl`

#  Size of long so we can distinguish 32 and 64 bit integer types. We assume
#  "int" is 32 bit in GAIA.
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
if test $ac_cv_sizeof_long_long -eq 8; then
   AC_DEFINE( INT64, [long long], [Type for 64 bit integers] )
fi
if test $ac_cv_sizeof_long -eq 8; then
   AC_DEFINE( INT64, [long], [Type for 64 bit integers] )
fi

]) 
#  GAIA3D_CONFIG


AC_DEFUN(GAIA3D_COMPILER_TESTS, [

#  Tests that require the full compiler setup, including CFLAGS.

#  Some compiler flags are set by tcl.m4, but are protected to survive
#  unexpanded into Makefile, work around that (tcl.4m should not be handling
#  CFLAGS at all, it should stick to another name and merge that in the
#  Makefile, CFLAGS are for users, not systems).  
old_CFLAGS="$CFLAGS"
CFLAGS=`eval echo "$CFLAGS"`

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

#  For accessing large files.
STAR_LARGEFILE_SUPPORT

])
#  GAIA3D_COMPILER_TESTS


dnl ========================================================================
dnl Author: Francesco Montorsi
dnl RCS-ID: $Id: vtk.m4,v 1.1 2005/11/20 14:47:40 frm Exp $
dnl
dnl Implements the AM_OPTIONS_VTK, to add the --with-vtk=path option, and the
dnl AM_PATH_VTK macro used to detect VTK presence, location and version.
dnl ========================================================================

dnl
dnl  AM_OPTIONS_VTK
dnl  ------------------------------------------------------------------------
dnl  Adds the --with-vtk=path option to the configure options
dnl
AC_DEFUN([AM_OPTIONS_VTK], [
AC_ARG_WITH([vtk], [AC_HELP_STRING([--with-vtk],
            [The prefix where VTK is installed (default is /usr)])],
            [with_vtk=$withval], [with_vtk="/usr"])
])


dnl
dnl  AM_PATH_VTK([minimum-version], [action-if-found], [action-if-not-found])
dnl  ------------------------------------------------------------------------
dnl
dnl  NOTE: [minimum-version] must be in the form [X.Y.Z]
dnl
AC_DEFUN([AM_PATH_VTK], [
dnl do we want to check for VTK ?
if test "$with_vtk" = "yes"; then

dnl in case user wrote --with-vtk=yes
   with_vtk="/usr"
fi

if test "$with_vtk" != "no"; then
   VTK_PREFIX="$with_vtk"

   AC_CHECK_FILE([$VTK_PREFIX/include/vtk/vtkCommonInstantiator.h], 
                 [vtkFound="OK"])
   AC_MSG_CHECKING([if VTK is installed in $VTK_PREFIX])

   if test -z "$vtkFound"; then
      AC_MSG_RESULT([no])
      $3
   else
      AC_MSG_RESULT([yes])

      dnl these are the VTK libraries of a default build
      VTK_LIBS="-lvtkCommon -lvtkDICOMParser -lvtkexpat -lvtkFiltering -lvtkfreetype -lvtkftgl -lvtkGraphics -lvtkHybrid -lvtkImaging -lvtkIO -lvtkjpeg -lvtkpng -lvtkRendering -lvtktiff -lvtkzlib"

      dnl set VTK cpp,ld flags
      VTK_CFLAGS="-I$VTK_PREFIX/include/vtk"
      VTK_CXXFLAGS="$VTK_CFLAGS"
      VTK_LDFLAGS="-L$VTK_PREFIX/lib/vtk $VTK_LIBS"

      dnl now, eventually check version
      if test -n "$1"; then

         dnl the version of VTK we need:
         maj=`echo $1 | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
         min=`echo $1 | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
         rel=`echo $1 | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
         AC_MSG_CHECKING([if VTK version is at least $maj.$min.$rel])

         dnl in order to be able to compile the following test program, we need
         dnl to add to the current flags, the VTK settings...

         OLD_CXXFLAGS=$CXXFLAGS
         OLD_LDFLAGS=$LDFLAGS
         CFLAGS="$VTK_CFLAGS $CFLAGS"
         CXXFLAGS="$VTK_CXXFLAGS $CXXFLAGS"
         LDFLAGS="$VTK_LDFLAGS $LDFLAGS"

         dnl check if the installed VTK is greater or not
         AC_LANG_PUSH([C++])
         AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <vtk/vtkVersion.h>], [
            printf("VTK version is: %d.%d.%d", VTK_MAJOR_VERSION, VTK_MINOR_VERSION, VTK_BUILD_VERSION);
            #if VTK_MAJOR_VERSION < $maj
            #error Installed VTK is too old !
            #endif
            #if VTK_MINOR_VERSION < $min
            #error Installed VTK is too old !
            #endif
            #if VTK_BUILD_VERSION < $rel
            #error Installed VTK is too old !
            #endif
            ])
         ], [vtkVersion="OK"])
         AC_LANG_POP([C++])

         if test "$vtkVersion" = "OK"; then
            AC_MSG_RESULT([yes])
            $2
         else
            AC_MSG_RESULT([no])

            dnl restore all flags without VTK values
            CFLAGS=$OLD_CFLAGS
            CXXFLAGS=$OLD_CXXFLAGS
            LDFLAGS=$OLD_LDFLAGS
            $3
         fi
      else
         dnl if we don't have to check for minimum version (because the user 
         dnl did not set that option), then we can execute here the block 
         dnl action-if-found
         CFLAGS="$VTK_CFLAGS $CFLAGS"
         CXXFLAGS="$VTK_CXXFLAGS $CXXFLAGS"
         LDFLAGS="$VTK_LDFLAGS $LDFLAGS"
         $2
      fi
   fi
fi
])
