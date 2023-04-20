dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(tclx, 8.4.1, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([tclx/configure.in])

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

STAR_DECLARE_DEPENDENCIES([build], [tcl])

dnl    Locate a suitable Starlink Tcl for this build.
STAR_PATH_TCLTK(8.6, tcl)

dnl    Allow user control over Tcl.
AC_ARG_WITH(tcl, [  --with-tcl              directory containing tcl configuration (tclConfig.sh)],
            with_tclconfig=${withval})

dnl    If no Tcl/Tk given explicitly use the Starlink versions.
if test "$with_tcl" = ""; then
   with_tcl="$TCL_PREFIX/lib"
fi

dnl    To configure TclX, run ./configure in the subdirectory.
dnl    Look for --disable-shared and pass it on.
star_extra_items=
for ac_arg in $ac_configure_args; do
   case $ac_arg in 
      *-disable-shared*)
         star_extra_items="--disable-shared"
      ;;
   esac
done
export star_extra_items
(
   cd tclx; \
   cmd="./configure --prefix=$prefix \
                    --srcdir=. \
                    $star_extra_items \
                    --cache-file=../config.cache \
                    --with-tcl=$with_tcl"; \
   echo $cmd; \
   $cmd; \
)
           
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd tclx; $(MAKE) DESTDIR=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
