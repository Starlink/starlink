#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

source [file join [file dirname [info script]] doxx.tcl]


p {
dnl	This file is an input file used by the GNU "autoconf" program to
dnl	generate the file "configure", which is run to configure the
dnl	Makefile in this directory.

AC_INIT(../../generic/tixInit.c)

#--------------------------------------------------------------------
#	Remove the ./config.cache file and rerun configure if
#	the cache file belong to a different architecture
#----------------------------------------------------------------------
AC_CHECK_PROG(UNAME, uname -a, [uname -a], "")
if test "$UNAME" = ""; then
    AC_CHECK_PROG(UNAME, uname, [uname], "")
fi

if test "$UNAME" != ""; then
    uname=`$UNAME`
    AC_MSG_CHECKING([cached value of \$uname])
    AC_CACHE_VAL(ac_cv_prog_uname, [nocached=1 ac_cv_prog_uname=`$UNAME`])
    if test "$nocached" = "1"; then
	AC_MSG_RESULT(no)
    else
	AC_MSG_RESULT(yes)
    fi

    if test "$uname" != "$ac_cv_prog_uname"; then
        echo "Running on a different machine/architecture. Can't use cached values"
	echo "Removing config.cache and running configure again ..."
	rm -f config.cache
        CMDLINE="$0 $*"
	exec $CMDLINE
    fi
fi

#----------------------------------------------------------------------
# We don't want to use any relative path because we need to generate
# Makefile's in subdirectories
#----------------------------------------------------------------------
if test "$INSTALL" = "./install.sh"; then
    INSTALL=`pwd`/install.sh
fi

#--------------------------------------------------------------------
#	Version information about this TIX release.
#--------------------------------------------------------------------

TIX_VERSION=4.1
TIX_MAJOR_VERSION=4
TIX_MINOR_VERSION=1
}

if !$ITCL {
    p {
BIN_VERSION=${TIX_VERSION}.@@_V_TCL_VER_@@
    }
} else {
    p {
BIN_VERSION=${TIX_VERSION}.@@_V_TCL_VER_@@.1
    }
}

p {

VERSION=${BIN_VERSION}

#--------------------------------------------------------------------
#	See if user wants to use gcc to compile Tix. This option must
#	be used before any checking that uses the C compiler.
#--------------------------------------------------------------------

AC_ARG_ENABLE(gcc, [  --enable-gcc            allow use of gcc if available],
    [tix_ok=$enableval], [tix_ok=no])
if test "$tix_ok" = "yes"; then
    AC_PROG_CC
else
    CC=${CC-cc}
AC_SUBST(CC)
fi

AC_PROG_INSTALL
AC_PROG_RANLIB
AC_HAVE_HEADERS(unistd.h limits.h)
AC_PROG_MAKE_SET

#--------------------------------------------------------------------
#	strdup() is not a POSIX call and many machines don't have it
#--------------------------------------------------------------------

AC_CHECK_FUNC(strdup, , AC_DEFINE(NO_STRDUP))

#--------------------------------------------------------------------
#	unsigned char is not supported by some non-ANSI compilers.
#--------------------------------------------------------------------

AC_MSG_CHECKING([unsigned char])
AC_TRY_COMPILE([#include <stdio.h>],[
     unsigned char c = 'c';
], tcl_ok=supported, tcl_ok="not supported")

AC_MSG_RESULT($tcl_ok)
if test "$tcl_ok" = supported; then
    AC_DEFINE(UCHAR_SUPPORTED)
fi

#--------------------------------------------------------------------
#	Check whether there is an strcasecmp function on this system.
#	This is a bit tricky because under SCO it's in -lsocket and
#	under Sequent Dynix it's in -linet.
#--------------------------------------------------------------------

AC_CHECK_FUNC(strcasecmp, tcl_ok=1, tcl_ok=0)
if test "$tcl_ok" = 0; then
    AC_CHECK_LIB(socket, strcasecmp, tcl_ok=1, tcl_ok=0)
fi
if test "$tcl_ok" = 0; then
    AC_CHECK_LIB(inet, strcasecmp, tcl_ok=1, tcl_ok=0)
fi
if test "$tcl_ok" = 0; then
    AC_DEFINE(NO_STRCASECMP)
fi
}

# LocatePkg75 --
#
#	This procedure is used to locate Tcl, Tk and ITcl packages for
#	tcl versions 7.5, 7.6, 7.7 and 8.0 and itcl 2.1, 2.2. It should be
#	able to work with a higher version of Tcl if its directory structure
#	is similar to Tcl 7.5~8.0
#
proc LocatePkg75 {} {
    global ITCL

set template {
#--------------------------------------------------------------------
#	See if there was a command-line option for where Pkg is;  if
#	not, assume that its top-level directory is a sibling of ours.
#--------------------------------------------------------------------

AC_ARG_WITH(pkg, [  --with-pkg=DIR           use Pkg @@_V_PKG_VER_@@ source from DIR],
   val=$withval, val="")

AC_MSG_CHECKING([Pkg source directory])

if test "$val" != ""; then
    PKG_SRC_DIR=$val
    if test ! -d $PKG_SRC_DIR; then
        AC_MSG_ERROR(Directory $PKG_SRC_DIR doesn't exist)
        AC_MSG_ERROR(Please install the source code of Pkg @@_V_PKG_VER_@@)
        exit 1
    fi
else
    dirs="../../../pkg@@_V_PKG_VER_@@"
    PKG_SRC_DIR="no-no"
    for i in $dirs; do
        if test -d $i; then
	    PKG_SRC_DIR=`cd $i; pwd`
        fi
    done

    if test ! -d $PKG_SRC_DIR; then
        AC_MSG_ERROR(Cannot locate Pkg source directory in $dirs)
        AC_MSG_ERROR(Please install the source code of Pkg @@_V_PKG_VER_@@)
        exit 1
    fi
fi
AC_MSG_RESULT($PKG_SRC_DIR)

PKG_BIN_DIR=$PKG_SRC_DIR/unix
}

if !$ITCL {
    #
    # Do Tcl
    #
    set str $template
    regsub -all PKG $str TCL str
    regsub -all Pkg $str Tcl str
    regsub -all pkg $str tcl str
    p $str

    #
    # Do Tk
    #
    set str $template
    regsub -all PKG $str TK str
    regsub -all Pkg $str Tk str
    regsub -all pkg $str tk str
    p $str
} else {
    #
    # Do ITcl
    #
    set str $template
    regsub -all PKG $str ITCL str
    regsub -all Pkg $str ITcl str
    regsub -all pkg $str itcl str
    p $str
    p {
ITCL_ROOT_DIR=$ITCL_SRC_DIR

TCL_SRC_DIR=$ITCL_ROOT_DIR/tcl@@_V_TCL_VER_@@ 
TK_SRC_DIR=$ITCL_ROOT_DIR/tk@@_V_TK_VER_@@ 

TCL_BIN_DIR=$TCL_SRC_DIR/unix
TK_BIN_DIR=$TK_SRC_DIR/unix
    }
}

}

LocatePkg75

p {
#--------------------------------------------------------------------
#	Find out the top level source directory of the Tix package.
#--------------------------------------------------------------------
TIX_SRC_DIR=`cd ../..; pwd`
}

p_sam {
#--------------------------------------------------------------------
#	See if we should compile SAM
#--------------------------------------------------------------------

AC_ARG_ENABLE(sam,
    [  --enable-sam            build stand-alone modules],
    [ok=$enableval], [ok=no])

if test "$ok" = "yes"; then
    TIX_BUILD_SAM="yes"
    TIX_SAM_TARGETS='$(SAM_TARGETS)'
else
    TIX_BUILD_SAM="no"
fi
}

if $ENABLE_SAM {
    if $SAM_EXE {
	p {
	    TIX_SAM_INSTALL="_install_sam_exe_ _install_sam_lib_"
	}
    } else {
	p {
	    TIX_SAM_INSTALL=_install_sam_lib_
	}
    }
}

##
## Tcl 7.4 --> we need to find the location of the X libs, etc.
##

p74 {
#--------------------------------------------------------------------
#	Supply a substitute for stdlib.h if it doesn't define strtol,
#	strtoul, or strtod (which it doesn't in some versions of SunOS).
#--------------------------------------------------------------------

AC_MSG_CHECKING(stdlib.h)
AC_HEADER_EGREP(strtol, stdlib.h, tk_ok=yes, tk_ok=no)
AC_HEADER_EGREP(strtoul, stdlib.h, , tk_ok=no)
AC_HEADER_EGREP(strtod, stdlib.h, , tk_ok=no)
if test $tk_ok = no; then
    AC_DEFINE(NO_STDLIB_H)
fi
AC_MSG_RESULT($tk_ok)

#--------------------------------------------------------------------
#	Check for various typedefs and provide substitutes if
#	they don't exist.
#--------------------------------------------------------------------

AC_MODE_T
AC_PID_T
AC_SIZE_T
AC_UID_T

#--------------------------------------------------------------------
#	Locate the X11 header files and the X11 library archive.  Try
#	the ac_path_x macro first, but if it doesn't find the X stuff
#	(e.g. because there's no xmkmf program) then check through
#	a list of possible directories.  Under some conditions the
#	autoconf macro will return an include directory that contains
#	no include files, so double-check its result just to be safe.
#--------------------------------------------------------------------

AC_PATH_X
not_really_there=""
if test "$no_x" = ""; then
    if test "$x_includes" = ""; then
	AC_TRY_CPP([#include <X11/XIntrinsic.h>], , not_really_there="yes")
    else
	if test ! -r $x_includes/X11/Intrinsic.h; then
	    not_really_there="yes"
	fi
    fi
fi
if test "$no_x" = "yes" -o "$not_really_there" = "yes"; then
    echo checking for X11 header files
    XINCLUDES="# no special path needed"
    AC_TRY_CPP([#include <X11/Intrinsic.h>], , XINCLUDES="nope")
    if test "$XINCLUDES" = nope; then
        dirs="/usr/unsupported/include /usr/local/include /usr/X386/include /usr/include/X11R4 /usr/X11R5/include /usr/include/X11R5 /usr/openwin/include /usr/X11/include /usr/sww/include"
        for i in $dirs ; do
	    if test -r $i/X11/Intrinsic.h; then
	        XINCLUDES=" -I$i"
	    fi
        done
    fi
else
    if test "$x_includes" != ""; then
	XINCLUDES=-I$x_includes
    else
	XINCLUDES="# no special path needed"
    fi
fi
if test "$XINCLUDES" = nope; then
  echo "Warning:  couldn't find any X11 include files."
  XINCLUDES="# no include files found"
fi
AC_SUBST(XINCLUDES)

if test "$no_x" = yes; then
    XLIBSW=nope
    if test "$XLIBSW" = nope; then
	dirs="/usr/unsupported/lib /usr/local/lib /usr/X386/lib /usr/lib/X11R4 /usr/X11R5/lib /usr/lib/X11R5 /usr/openwin/lib /usr/X11/lib /usr/sww/X11/lib"
	for i in $dirs ; do
	    if test -r $i/libX11.a -o -r $i/libX11.so -o -r $i/libX11.sl; then
		XLIBSW="-L$i -lX11"
	    fi
	done
    fi
else
    if test "$x_libraries" = ""; then
	XLIBSW=-lX11
    else
	XLIBSW="-L$x_libraries -lX11"
    fi
fi
if test "$XLIBSW" = nope ; then
    AC_CHECK_LIB(Xwindow, XCreateWindow, XLIBSW=-lXwindow)
fi
if test "$XLIBSW" = nope ; then
    echo "Warning:  couldn't find the X11 library archive.  Using -lX11."
    XLIBSW=-lX11
fi
AC_SUBST(XLIBSW)

#--------------------------------------------------------------------
#	Check for the existence of various libraries.  The order here
#	is important, so that then end up in the right order in the
#	command line generated by make.  The -lsocket and -lnsl libraries
#	require a couple of special tricks:
#	1. Use "connect" and "accept" to check for -lsocket, and
#	   "gethostbyname" to check for -lnsl.
#	2. Use each function name only once:  can't redo a check because
#	   autoconf caches the results of the last check and won't redo it.
#	3. Use -lnsl and -lsocket only if they supply procedures that
#	   aren't already present in the normal libraries.  This is because
#	   IRIX 5.2 has libraries, but they aren't needed and they're
#	   bogus:  they goof up name resolution if used.
#	4. On some SVR4 systems, can't use -lsocket without -lnsl too.
#	   To get around this problem, check for both libraries together
#	   if -lsocket doesn't work by itself.
#--------------------------------------------------------------------

AC_CHECK_LIB(Xbsd, main, [LIBS="$LIBS -lXbsd"])

tk_checkBoth=0
AC_CHECK_FUNC(connect, tk_checkSocket=0, tk_checkSocket=1)
if test "$tk_checkSocket" = 1; then
    AC_CHECK_LIB(socket, main, LIBS="$LIBS -lsocket", tk_checkBoth=1)
fi
if test "$tk_checkBoth" = 1; then
    tk_oldLibs=$LIBS
    LIBS="$LIBS -lsocket -lnsl"
    AC_CHECK_FUNC(accept, tk_checkNsl=0, [LIBS=$tk_oldLibs])
fi
AC_CHECK_FUNC(gethostbyname, , AC_CHECK_LIB(nsl, main, [LIBS="$LIBS -lnsl"]))

#--------------------------------------------------------------------
# One more check related to the X libraries.  The standard releases
# of Ultrix don't support the "xauth" mechanism, so send won't work
# unless TK_NO_SECURITY is defined.  However, there are usually copies
# of the MIT X server available as well, which do support xauth.
# Check for the MIT stuff and use it if it exists.
#
# Note: can't use ac_check_lib macro (at least, not in Autoconf 2.1)
# because it can't deal with the "-" in the library name.
#--------------------------------------------------------------------

if test -d /usr/include/mit ; then
    AC_MSG_CHECKING([MIT X libraries])
    tk_oldCFlags=$CFLAGS
    CFLAGS="$CFLAGS -I/usr/include/mit"
    tk_oldLibs=$LIBS
    LIBS="$LIBS -lX11-mit"
    AC_TRY_LINK([
	#include <X11/Xlib.h>
    ], [
	XOpenDisplay(0);
    ], [
	AC_MSG_RESULT(yes)
	XLIBSW="-lX11-mit"
	XINCLUDES="-I/usr/include/mit"
    ], AC_MSG_RESULT(no))
    CFLAGS=$tk_oldCFlags
    LIBS=$tk_oldLibs
fi

#--------------------------------------------------------------------
#	On a few very rare systems, all of the libm.a stuff is
#	already in libc.a.  Set compiler flags accordingly.
#	Also, Linux requires the "ieee" library for math to
#	work right (and it must appear before "-lm").
#--------------------------------------------------------------------

MATH_LIBS=""
AC_CHECK_FUNC(sin, , MATH_LIBS="-lm")
AC_CHECK_LIB(ieee, main, [MATH_LIBS="-lieee $MATH_LIBS"])
AC_SUBST(MATH_LIBS)

#--------------------------------------------------------------------
#	If this system doesn't have a memmove procedure, use memcpy
#	instead.
#--------------------------------------------------------------------

AC_CHECK_FUNC(memmove, , [AC_DEFINE(memmove, memcpy)])
}

ptcl {
IS_ITCL=0
ITCL_BUILD_LIB_SPEC=""
ITK_BUILD_LIB_SPEC=""
TIX_EXE_FILE=tixwish
TCL_SAMEXE_FILE=satclsh
TK_SAMEXE_FILE=sawish
TIX_SAMEXE_FILE=satixwish
}

pitcl {
AC_DEFINE(ITCL_2)
IS_ITCL=1
TIX_EXE_FILE=tixwish
TCL_SAMEXE_FILE=satclsh_not_supported
TK_SAMEXE_FILE=sawish_not_supported
TIX_SAMEXE_FILE=satixwish_not_supported
}

p74 {
TIX_LIB_FILE=lib@@_V_LNAME_@@4140@@_V_BVEREXT_@@.a
TIX_MAKE_LIB="ar cr ${TIX_LIB_FILE} \${OBJS}"
TIX_BUILD_LIB_SPEC="\$(TIX_LIB_FILE)"
TCL_SAM_FILE=libtclsam74.a
TK_SAM_FILE=libtksam40.a
TIX_SAM_FILE=lib@@_V_LNAME_@@sam4140@@_V_BVEREXT_@@.a
TCL_MAKE_SAM="ar cr ${TCL_SAM_FILE} \${TCL_SAM_OBJS}"
TK_MAKE_SAM="ar cr ${TK_SAM_FILE} \${TK_SAM_OBJS}"
TIX_MAKE_SAM="ar cr ${TIX_SAM_FILE} \${TIX_SAM_OBJS}"
TCL_BUILD_SAM_SPEC="\$(TCL_SAM_FILE)"
TK_BUILD_SAM_SPEC="\$(TK_SAM_FILE)"
TIX_BUILD_SAM_SPEC="\$(TIX_SAM_FILE)"
}

if {$ITCL && $subs(@@_V_ITCL_VER_@@) == "2.0"} {
    p {
ITCL_BUILD_LIB_SPEC="\$(ITCL_ROOT_DIR)/itcl/libitcl.a"
ITK_BUILD_LIB_SPEC="\$(ITCL_ROOT_DIR)/itk/libitk.a"
    }
}

p75+ {
#--------------------------------------------------------------------
#	Read in configuration information generated by Tcl for shared
#	libraries, and arrange for it to be substituted into our
#	Makefile.
#--------------------------------------------------------------------

file=$TCL_BIN_DIR/tclConfig.sh
. $file
CC=$TCL_CC
SHLIB_CFLAGS=$TCL_SHLIB_CFLAGS
SHLIB_LD=$TCL_SHLIB_LD
SHLIB_LD_LIBS=$TCL_SHLIB_LD_LIBS
SHLIB_SUFFIX=$TCL_SHLIB_SUFFIX
SHLIB_VERSION=$TCL_SHLIB_VERSION

DL_LIBS=$TCL_DL_LIBS
LD_FLAGS=$TCL_LD_FLAGS
TIX_LD_SEARCH_FLAGS=$TCL_LD_SEARCH_FLAGS

#--------------------------------------------------------------------
#	Read in configuration information generated by Tk and arrange
#	for it to be substituted into our Makefile.
#--------------------------------------------------------------------
file=$TK_BIN_DIR/tkConfig.sh
. $file

TIX_DEFS="$TK_DEFS $TCL_DEFS"

# Note:  in the following variable, it's important to use the absolute
# path name of the Tcl directory rather than "..":  this is because
# AIX remembers this path and will attempt to use it at run-time to look
# up the Tcl library.

if test "${TCL_LIB_VERSIONS_OK}" = "ok"; then
    TIX_BUILD_LIB_SPEC="-L`pwd` -l@@_V_LNAME_@@${VERSION}"
    TIX_BUILD_SAM_SPEC="-L`pwd` -l@@_V_LNAME_@@sam${VERSION}"
    TCL_BUILD_SAM_SPEC="-L`pwd` -ltclsam${TCL_VERSION}"
    TK_BUILD_SAM_SPEC="-L`pwd` -ltksam${TK_VERSION}"
    TIX_LIB_SPEC="-L${exec_prefix}/lib -l@@_V_LNAME_@@${VERSION}"
else
    TIX_BUILD_LIB_SPEC="-L`pwd` -l@@_V_LNAME_@@`echo ${VERSION} | tr -d .`"
    TIX_BUILD_SAM_SPEC="-L`pwd` -l@@_V_LNAME_@@sam`echo ${VERSION} | tr -d .`"
    TCL_BUILD_SAM_SPEC="-L`pwd` -ltclsam`echo ${TCL_VERSION} | tr -d .`"
    TK_BUILD_SAM_SPEC="-L`pwd` -ltksam`echo ${TK_VERSION} | tr -d .`"
    TIX_LIB_SPEC="-L${exec_prefix}/lib -l@@_V_LNAME_@@`echo ${VERSION} | tr -d .`"
fi

#--------------------------------------------------------------------
#	See if we should compile shared library.
#--------------------------------------------------------------------

AC_ARG_ENABLE(shared,
    [  --enable-shared         build lib@@_V_LNAME_@@ as a shared library],
    [ok=$enableval], [ok=no])

if test "$ok" = "yes" -a "${SHLIB_SUFFIX}" != ""; then
    TIX_SHLIB_CFLAGS="${SHLIB_CFLAGS}"
    RANLIB=":"

    # The main Tix library
    #
    eval "TIX_LIB_FILE=lib@@_V_LNAME_@@${TCL_SHARED_LIB_SUFFIX}"
    TIX_MAKE_LIB="\${SHLIB_LD} -o ${TIX_LIB_FILE} \${OBJS} ${SHLIB_LD_LIBS}"

    # The Tcl SAM library
    #
    VERSION=@@_V_TCL_VER_@@
    eval "TCL_SAM_FILE=libtclsam${TCL_SHARED_LIB_SUFFIX}"
    TCL_MAKE_SAM="\${SHLIB_LD} -o ${TCL_SAM_FILE} \${TCL_SAM_OBJS} ${SHLIB_LD_LIBS}"
    
    # The Tk SAM library
    #
    VERSION=@@_V_TK_VER_@@
    eval "TK_SAM_FILE=libtksam${TCL_SHARED_LIB_SUFFIX}"
    TK_MAKE_SAM="\${SHLIB_LD} -o ${TK_SAM_FILE} \${TK_SAM_OBJS} ${SHLIB_LD_LIBS}"
    
    # The Tix SAM library
    #
    VERSION=${BIN_VERSION}
    eval "TIX_SAM_FILE=lib@@_V_LNAME_@@sam${TCL_SHARED_LIB_SUFFIX}"
    TIX_MAKE_SAM="\${SHLIB_LD} -o ${TIX_SAM_FILE} \${TIX_SAM_OBJS} ${SHLIB_LD_LIBS}"

else
    TIX_SHLIB_CFLAGS=""

    # The main Tix library
    #
    eval "TIX_LIB_FILE=lib@@_V_LNAME_@@${TCL_UNSHARED_LIB_SUFFIX}"
    TIX_MAKE_LIB="ar cr ${TIX_LIB_FILE} \${OBJS}"

    # The Tcl SAM library
    
    VERSION=@@_V_TCL_VER_@@
    eval "TCL_SAM_FILE=libtclsam${TCL_UNSHARED_LIB_SUFFIX}"
    TCL_MAKE_SAM="ar cr ${TCL_SAM_FILE} \${TCL_SAM_OBJS}"
    
    # The Tk SAM library
    #
    VERSION=@@_V_TK_VER_@@
    eval "TK_SAM_FILE=libtksam${TCL_UNSHARED_LIB_SUFFIX}"
    TK_MAKE_SAM="ar cr ${TK_SAM_FILE} \${TK_SAM_OBJS}"
    
    # The Tix SAM library
    #
    VERSION=${BIN_VERSION}
    eval "TIX_SAM_FILE=lib@@_V_LNAME_@@sam${TCL_UNSHARED_LIB_SUFFIX}"
    TIX_MAKE_SAM="ar cr ${TIX_SAM_FILE} \${TIX_SAM_OBJS}"
fi
}

if {$ITCL && $subs(@@_V_ITCL_VER_@@) != "2.0"} {
    p {
#--------------------------------------------------------------------
#	Read in configuration information generated by ITcl
#	and arrange for it to be substituted into our Makefile.
#--------------------------------------------------------------------
file=$ITCL_ROOT_DIR/itcl/unix/itclConfig.sh
. $file
    }
    if {$subs(@@_V_ITCL_VER_@@) > 2.1} {
	p {
#--------------------------------------------------------------------
#	Read in configuration information generated by ITk
#	and arrange for it to be substituted into our Makefile.
#--------------------------------------------------------------------
file=$ITCL_ROOT_DIR/itk/unix/itkConfig.sh
. $file
        }
    } else {
	p {
#----------------------------------------------------------------------
#	The ITK_BUILD_LIB_SPEC is incorrect in Itcl 2.1
#----------------------------------------------------------------------
ITK_BUILD_LIB_SPEC="-L\$(ITCL_ROOT_DIR)/itk/unix ${ITK_BUILD_LIB_SPEC}"
	}
    }
}

p75+ {

#--------------------------------------------------------------------
#	Check for the existence of the -lsocket and -lnsl libraries.
#	The order here is important, so that they end up in the right
#	order in the command line generated by make.  Here are some
#	special considerations:
#	1. Use "connect" and "accept" to check for -lsocket, and
#	   "gethostbyname" to check for -lnsl.
#	2. Use each function name only once:  can't redo a check because
#	   autoconf caches the results of the last check and won't redo it.
#	3. Use -lnsl and -lsocket only if they supply procedures that
#	   aren't already present in the normal libraries.  This is because
#	   IRIX 5.2 has libraries, but they aren't needed and they're
#	   bogus:  they goof up name resolution if used.
#	4. On some SVR4 systems, can't use -lsocket without -lnsl too.
#	   To get around this problem, check for both libraries together
#	   if -lsocket doesn't work by itself.
#--------------------------------------------------------------------

checked=0
for i in $TK_LIBS; do
    if test "$i" = "-lsocket"; then
	checked=1
    fi
done

if test "$checked" = "0"; then
    tcl_checkBoth=0
    AC_CHECK_FUNC(connect, tcl_checkSocket=0, tcl_checkSocket=1)
    if test "$tcl_checkSocket" = 1; then
        AC_CHECK_LIB(socket, main, TK_LIBS="$TK_LIBS -lsocket",
	    tcl_checkBoth=1)
    fi
    if test "$tcl_checkBoth" = 1; then
        tk_oldLibs=$TK_LIBS
        TK_LIBS="$TK_LIBS -lsocket -lnsl"
        AC_CHECK_FUNC(accept, tcl_checkNsl=0, [TK_LIBS=$tk_oldLibs])
    fi
    AC_CHECK_FUNC(gethostbyname, , AC_CHECK_LIB(nsl, main,
        [TK_LIBS="$TK_LIBS -lnsl"]))
fi
}

p {
#----------------------------------------------------------------------
#	Substitution strings exported by TIX
#----------------------------------------------------------------------
AC_SUBST(CC)
AC_SUBST(RANLIB)
AC_SUBST(SHLIB_CFLAGS)
AC_SUBST(SHLIB_LD)
AC_SUBST(SHLIB_LD_LIBS)
AC_SUBST(SHLIB_SUFFIX)
AC_SUBST(SHLIB_VERSION)
AC_SUBST(DL_LIBS)
AC_SUBST(LD_FLAGS)
AC_SUBST(TCL_BUILD_LIB_SPEC)
AC_SUBST(TCL_LIBS)
AC_SUBST(TCL_VERSION)
AC_SUBST(TCL_SRC_DIR)
AC_SUBST(TCL_BIN_DIR)
AC_SUBST(TK_BUILD_LIB_SPEC)
AC_SUBST(TK_LIBS)
AC_SUBST(TK_VERSION)
AC_SUBST(TK_SRC_DIR)
AC_SUBST(TK_BIN_DIR)
AC_SUBST(TK_XINCLUDES)
AC_SUBST(TIX_LD_SEARCH_FLAGS)
AC_SUBST(TIX_MAJOR_VERSION)
AC_SUBST(TIX_MINOR_VERSION)
AC_SUBST(TIX_VERSION)
AC_SUBST(TIX_SRC_DIR)
AC_SUBST(TIX_SHLIB_CFLAGS)
AC_SUBST(TIX_MAKE_LIB)
AC_SUBST(TIX_LIB_FILE)
AC_SUBST(TIX_BUILD_LIB_SPEC)
AC_SUBST(TIX_LIB_SPEC)
AC_SUBST(TIX_EXE_FILE)
AC_SUBST(TIX_SAM_TARGETS)
AC_SUBST(TIX_SAM_INSTALL)
AC_SUBST(TCL_SAM_FILE)
AC_SUBST(TCL_MAKE_SAM)
AC_SUBST(TK_SAM_FILE)
AC_SUBST(TK_MAKE_SAM)
AC_SUBST(TIX_SAM_FILE)
AC_SUBST(TIX_MAKE_SAM)
AC_SUBST(TIX_DEFS)
AC_SUBST(ITCL_BUILD_LIB_SPEC)
AC_SUBST(ITK_BUILD_LIB_SPEC)
AC_SUBST(TCL_SAMEXE_FILE)
AC_SUBST(TK_SAMEXE_FILE)
AC_SUBST(TIX_SAMEXE_FILE)
AC_SUBST(TCL_BUILD_SAM_SPEC)
AC_SUBST(TK_BUILD_SAM_SPEC)
AC_SUBST(TIX_BUILD_SAM_SPEC)
}

p75+ {
# The "binary version" of Tix (see docs/Pkg.txt)
TIX_VERSION_PKG=${BIN_VERSION}
AC_SUBST(TIX_VERSION_PKG)
}

if !$TCL74 {
    p_sam {
TIXSAM_PKG_FILE="[[file join [file dirname \$dir] ${TIX_SAM_FILE}]]"
if test "$TIX_BUILD_SAM" = "yes"; then
    TIX_SAM_PACKAGE_IFNEEDED="package ifneeded Tixsam ${TIX_VERSION_PKG} [[list load \"${TIXSAM_PKG_FILE}\" Tixsam]]"
fi
    }
}

p75+ {
# The package file, usually a shared library
TIX_PKG_FILE="[[file join [file dirname \$dir] ${TIX_LIB_FILE}]]"
AC_SUBST(TIX_PKG_FILE)
AC_SUBST(TIX_SAM_PACKAGE_IFNEEDED)
}




pitcl {
AC_SUBST(ITCL_ROOT_DIR)
}

p74 {
AC_OUTPUT(Makefile)
}

p75+ {
AC_OUTPUT(Makefile pkgIndex.tcl)
}
