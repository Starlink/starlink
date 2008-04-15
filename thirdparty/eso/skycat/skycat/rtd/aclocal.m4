builtin(include,../tclconfig/tcl.m4)

AC_DEFUN(RTD_CONFIG, [

# -----------------------------------------------------------------------
# Load the Tclutil definitions
cf=../tclutil/tclutilConfig.sh
if test -f $cf ; then
    . $cf
    AC_SUBST(tclutil_VERSION)
    AC_SUBST(tclutil_LIB_FILE)
    AC_SUBST(tclutil_BUILD_LIB_SPEC)
    AC_SUBST(tclutil_BUILD_DIR)
    AC_SUBST(tclutil_LIB_SPEC)
    AC_SUBST(BLT_LIB_DIR)
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
# Optionally merge object and header files from dependent packages to make one master rtd lib
MERGED=1
AC_ARG_ENABLE(merge, 
    [AC_HELP_STRING([--enable-merge],[merge the contents of dependent packages into a master library])],
    [MERGED=$enableval],
    [MERGED=no])

tclsources=`cd $srcdir; echo library/*.tcl`

changequote(<<, >>)
csources=`cd $srcdir; echo generic/*.[Cc] rtdevt/rtdImageEvent.c rtdevt/rtdSem.c`
changequote([, ])

rtd_headers=`cd $srcdir; echo generic/*.h generic/*.icc rtdevt/rtdImageEvent.h rtdevt/rtdSem.h`
astrotcl_headers=`cd $srcdir; echo ../astrotcl/{generic,press,libwcs,cfitsio}/*.h`
tclutil_headers=`cd $srcdir; echo ../tclutil/generic/*.h`

rtd_includes="-I$srcdir/generic -I$srcdir/rtdevt -I$srcdir/bitmaps"
astrotcl_includes="-I$srcdir/../astrotcl/generic -I$srcdir/../astrotcl/cfitsio -I$srcdir/../astrotcl/libwcs"
tclutil_includes="-I$srcdir/../tclutil/generic"
cincludes="${rtd_includes} ${astrotcl_includes} ${tclutil_includes}"

if test $MERGED = yes ; then
    echo "Will build merged master rtd library"
    cheaders="${rtd_headers} ${astrotcl_headers} ${tclutil_headers}"
    MERGE_OBJECTS="$astrotcl_PKG_OBJECTS $tclutil_PKG_OBJECTS"
dnl     AC_DEFINE(MERGE_OBJECTS, 1, [merge the contents of dependent packages into a master library])
else
    echo "Not making a merged master rtd library"
    cheaders="${rtd_headers}"
    MERGE_OBJECTS=""
fi
AC_SUBST(MERGE_OBJECTS)


# -----------------------------------------------------------------------
AC_DEFINE(USE_COMPAT_CONST, 1, [For compatibility between tcl8.4 and previous tcl releases])

# -----------------------------------------------------------------------
AC_MSG_CHECKING([sysv shared memory prototypes])
AC_EGREP_HEADER([int.*shmdt.*\(], [sys/shm.h], 
	[AC_MSG_RESULT(yes)], 
	[AC_MSG_RESULT(no); AC_DEFINE(NEED_SHM_PROTO, 1, 
		[Check if we need (or can use) shared memory (sysv/shm) prototypes])])

# -----------------------------------------------------------------------
AC_MSG_CHECKING([gethostname prototype])
AC_EGREP_HEADER([int.*gethostname.*\(], [unistd.h], 
	[AC_MSG_RESULT(yes)], 
	[AC_MSG_RESULT(no); AC_DEFINE(NEED_GETHOSTNAME_PROTO, 1, 
		[Check if we need a prototype for gethostname()])])


# -----------------------------------------------------------------------
AC_CHECK_SIZEOF(long, 4)


# -----------------------------------------------------------------------
# there are some idiosyncrasies with semun defs (used in semxxx). Solaris
# does not define it at all
# -------------------------------------------------------------------------

AC_MSG_CHECKING("do we have union semun defined")
AC_TRY_COMPILE(
[#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
], [
union semun filler;
], [
AC_DEFINE(HAVE_UNION_SEMUN)   
AC_MSG_RESULT("yes")
], AC_MSG_RESULT("no") )

AC_DEFINE(HAVE_NET_SERVICES)
AC_CHECK_HEADERS(sys/filio.h)

#  Check if we need (or can use) the socklen_t type.
AC_CHECK_TYPES([socklen_t],,,[#include <sys/socket.h>])

#------------------------------------------------------------------------
AC_LANG(C++)
AC_MSG_CHECKING([fd_set])
AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>],
	[fd_set readFds; select(32, &readFds, 0, 0, 0);], test_ok=yes, test_ok=no)
if test $test_ok = yes; then
	AC_DEFINE(HAVE_SELECT_FD_SET, 1, 
		[See if the select system call uses fd_set arguments])
fi
AC_MSG_RESULT($test_ok)

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
])


