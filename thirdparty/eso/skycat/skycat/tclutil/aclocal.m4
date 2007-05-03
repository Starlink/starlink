builtin(include,../tclconfig/tcl.m4)

AC_DEFUN(TCLUTIL_CONFIG, [

#------------------------------------------------------------------------
# TCLUTIL_PATH_BLT --
#
#	Locate the BLT library
#
# Arguments:
#	none
#
# Results:
#
#	Adds the following arguments to configure:
#		--with-blt=...
#
#	Defines the following vars:
#		BLT_LIB_SPEC      String to add to link the BLT lib (-L... -lBLT)
#		BLT_LIB_DIR       Directory containing libBLT24.so
#------------------------------------------------------------------------

AC_DEFUN(TCLUTIL_PATH_BLT, [
    AC_MSG_CHECKING(for BLT library)
    AC_ARG_WITH(blt,
       [AC_HELP_STRING([--with-blt=DIR],[link with BLT library installed in DIR])],
       BLT_LIB_DIR=$withval)

    BLT_LIBNAME=libBLT24${SHLIB_SUFFIX}
    BLT_LIBFLAG=-lBLT24

    if test -z "$BLT_LIB_DIR" ; then
	# If --with-blt=dir was not specified, try the Tcl lib dir and the exec-prefix/lib dir
       	places="\
		$TCL_BIN_DIR \
		$TCL_BIN_DIR/blt2.4 \
		$TCLTK_ROOT/lib \
		$TCLTK_ROOT/lib/blt2.4 \
		$exec_prefix/lib \
		$exec_prefix/lib/blt2.4 \
		$prefix/lib \
		$prefix/lib/blt2.4 \
	" 
	for i in $places ; do 
		if test -f $i/$BLT_LIBNAME 
		then
       			BLT_LIB_DIR=$i
			break
		fi
	done
        if test -z "$BLT_LIB_DIR" ; then
	     echo
	     AC_MSG_ERROR([could not find $BLT_LIBNAME: Please use the --with-blt=DIR option.])
	fi
    else 
       # Check if the BLT library is in the lib subdir of the given dir
       if test ! -f $BLT_LIB_DIR/$BLT_LIBNAME
       then
          if test ! -f $BLT_LIB_DIR/lib/$BLT_LIBNAME
          then
	     echo
	     AC_MSG_ERROR([could not find $BLT_LIBNAME in $BLT_LIB_DIR or in $BLT_LIB_DIR/lib: Please use the --with-blt=DIR option.])
          else
	    BLT_LIB_DIR=$BLT_LIB_DIR/lib
          fi
       fi
    fi
    BLT_LIB_SPEC="-L$BLT_LIB_DIR $BLT_LIBFLAG"
    AC_MSG_RESULT($BLT_LIB_DIR)
    AC_SUBST(BLT_LIB_DIR)
    AC_SUBST(BLT_LIB_SPEC)
])

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

# -----------------------------------------------------------------------
AC_MSG_CHECKING([mmap prototypes])
AC_EGREP_HEADER([int.*munmap.*\(], [sys/mman.h], 
	[AC_MSG_RESULT(yes)], 
	[AC_MSG_RESULT(no); AC_DEFINE(NEED_MMAP_PROTO, 1, 
		[Check if we need (or can use) mmap prototypes])])

# -----------------------------------------------------------------------
AC_CHECK_HEADERS(sys/filio.h)
AC_CHECK_HEADERS(sys/statvfs.h)

# -----------------------------------------------------------------------
# 	Check if we need (or can use) the socklen_t type.
# -----------------------------------------------------------------------
AC_CHECK_TYPES([socklen_t],,,[#include <sys/socket.h>])

#------------------------------------------------------------------------
#AC_LANG(C++)
AC_MSG_CHECKING([fd_set])
AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/time.h>],
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
      SHLIB_LD_CXX_LIBS="/usr/lib/libCrun.so.1 /usr/lib/libCstd.so.1"
   ;;
   OSF*)
      SHLIB_LD_CXX_LIBS="-lcxx -lcxxstd"
   ;;
esac
echo "SHLIB_LD_CXX_LIBS = $SHLIB_LD_CXX_LIBS"
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

]) #  End of macro

