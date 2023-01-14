dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(tkimg, 1.3.0, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([tkimg/configure.in])

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

STAR_DECLARE_DEPENDENCIES([build], [tcl tk])

dnl    Locate a suitable Starlink Tcl/Tk for this build.
STAR_PATH_TCLTK(8.6, tk)

dnl    Allow user control over Tcl and Tk.
AC_ARG_WITH(tcl, [  --with-tcl              directory containing tcl configuration (tclConfig.sh)],
            with_tclconfig=${withval})
AC_ARG_WITH(tk, [  --with-tk              directory containing tk configuration (tkConfig.sh)],
            with_tkconfig=${withval})

dnl    If no Tcl/Tk given explicitly use the Starlink versions.
if test "$with_tcl" = ""; then
   with_tcl="$TCL_PREFIX/lib"
fi
if test "$with_tk" = ""; then
   with_tk="$TCL_PREFIX/lib"
fi

dnl    To configure TkImg, run ./configure in the subdirectory, passing on any
dnl    command-line values. 
(
   cd tkimg; \
   cmd="./configure --prefix=$prefix \
                    --srcdir=. \
                    --cache-file=../../config.cache \
                    --with-tcl=$with_tcl \
                    --with-tk=$with_tk \
                    $ac_configure_args"; \
   echo $cmd; \
   eval $cmd; \
)
           
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd tkimg; $(MAKE) DESTDIR=$$DESTDIR INSTALL_ROOT=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
