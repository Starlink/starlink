builtin(include,../tclconfig/tcl.m4)

AC_DEFUN(ASTROTCL_CONFIG, [

# Load the Tclutil definitions
cf=../tclutil/tclutilConfig.sh
if test -f $cf ; then
    . $cf
    AC_SUBST(tclutil_VERSION)
    AC_SUBST(tclutil_LIB_FILE)
    AC_SUBST(tclutil_BUILD_LIB_SPEC)
    AC_SUBST(tclutil_LIB_SPEC)
    AC_SUBST(BLT_LIB_SPEC)
    AC_SUBST(tclutil_SRC_DIR)
else
    AC_MSG_ERROR([$cf doesn't exist])
fi

AC_CHECK_HEADERS(sys/filio.h)

#  Check if we need (or can use) the socklen_t type.
AC_CHECK_TYPES([socklen_t],,,[#include <sys/socket.h>])

AC_DEFINE(USE_COMPAT_CONST, 1, [For compatibility between tcl8.4 and previous tcl releases])

#--------------------------------------------------------------------
# From the cfitsio configure script
#--------------------------------------------------------------------
AC_CHECK_HEADERS(stdlib.h string.h math.h limits.h ,ANSI_HEADER=yes,ANSI_HEADER=no)dnl

# ================= test for the unix ftruncate function ================

AC_MSG_CHECKING("whether ftruncate works")
AC_TRY_LINK([#include <unistd.h>
], [
ftruncate(0, 0);
], [
AC_DEFINE(HAVE_FTRUNCATE)
AC_MSG_RESULT("yes")
], AC_MSG_RESULT("no") )

# ---------------------------------------------------------
# some systems define long long for 64-bit ints
# ---------------------------------------------------------
 
AC_MSG_CHECKING("whether long long is defined")
AC_TRY_COMPILE([#include <stdlib.h>
], [
long long filler;
], [
AC_DEFINE(HAVE_LONGLONG)
AC_MSG_RESULT("yes")
], AC_MSG_RESULT("no") )

# -------------------------------------------------------------------------
# check is System V IPC is supported on this machine
# -------------------------------------------------------------------------

AC_MSG_CHECKING("whether system V style IPC services are supported")
AC_TRY_LINK([#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
], [
shmat(0, 0, 0);
shmdt(0);
shmget(0, 0, 0);
semget(0, 0, 0);
], [
AC_DEFINE(HAVE_SHMEM_SERVICES)
my_shmem=\${SOURCES_SHMEM}
AC_MSG_RESULT("yes")
], AC_MSG_RESULT("no") )

AC_SUBST(my_shmem)

# -------------------------------------------------------------------------
# some systems define flock_t, for others we have to define it ourselves
# -------------------------------------------------------------------------

AC_MSG_CHECKING("do we have flock_t defined in sys/fcntl.h")
AC_TRY_COMPILE([#include <sys/fcntl.h>
], [
flock_t filler;
], [
AC_DEFINE(HAVE_FLOCK_T)
AC_MSG_RESULT("yes") 
], AC_MSG_RESULT("no") )

if test "$HAVE_FLOCK_T" != 1; then
   AC_MSG_CHECKING("do we have flock_t defined in sys/flock.h")
   AC_TRY_COMPILE([#include <sys/flock.h>
   ], [
   flock_t filler;
   ], [
   AC_DEFINE(HAVE_FLOCK_T)
   AC_MSG_RESULT("yes") 
   ], AC_MSG_RESULT("no") )
fi

# -------------------------------------------------------------------------
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

# ==================== END OF cfitsio SECTION ================

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

