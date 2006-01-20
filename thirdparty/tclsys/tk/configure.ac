dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(tk, 8.2.3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

STAR_DECLARE_DEPENDENCIES([build], [tcl])

dnl    May invoke make recursively from Makefile.am
AC_PROG_MAKE_SET

AC_ARG_WITH([tcl], [  --with-tcl              directory containing tcl configuration (tclConfig.sh)],
            [with_tclconfig=yes],[with_tclconfig=no])
if test "X$with_tcl" != "X"; then
   TCL_CONFIG_DIR="--with-tcl=$with_tcl"
else 
   TCL_CONFIG_DIR=""
fi
export TCL_CONFIG_DIR

dnl    Check for X libraries. These should be passed on to subdirs.
AC_PATH_X
if test "X$x_includes" != "X"; then
   X11_INCLUDES="--x-includes=$x_includes"
else
   X11_INCLUDES=""
fi
export X11_INCLUDES
if test "X$x_libraries" != "X"; then
   X11_LIBRARIES="--x-libraries=$x_libraries"
else
   X11_LIBRARIES=""
fi
export X11_LIBRARIES

dnl    To configure Tcl, run ./configure in the `unix' directory.
dnl    Do not invoke AC_CONFIG_SUBDIRS, since that prompts autoreconf
dnl    to try to reconfigure that directory, and automake to assume
dnl    it's allowed to play there, too.
(
    cd unix
    echo ./configure --prefix=$prefix --cache-file=config.cache "$TCL_CONFIG_DIR" "$X11_LIBRARIES" "$X11_INCLUDES"
         ./configure --prefix=$prefix --cache-file=config.cache "$TCL_CONFIG_DIR" "$X11_LIBRARIES" "$X11_INCLUDES"
)

STAR_SPECIAL_INSTALL_COMMAND([cd unix;
    $(MAKE) INSTALL_ROOT=$$DESTDIR install])

AC_CONFIG_FILES([Makefile component.xml make.config])

AC_OUTPUT

