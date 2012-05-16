dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 24276 $)

dnl    Initialisation: package name and version number
AC_INIT(vtk, 5.10.0, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Get extension for shared libraries.
dnl    Need to determine the suffix for shareable libraries, as used by
dnl    Tcl, so we can link against the GAIA dynamic library.
tcl_system=`uname -s`-`uname -r`
case $tcl_system in
   windows)
      TCL_SHLIB_EXT='.dll'
      ;;
   Darwin-*)
      TCL_SHLIB_EXT='.dylib'
      ;;
   *)
      TCL_SHLIB_EXT='.so'
      ;;
esac
AC_SUBST(TCL_SHLIB_EXT)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([VTK/CMakeLists.txt])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Dependencies. Main one is Mesa which ensures we have an OpenGL
dnl    API, but that's a stub on machines with hardware OpenGL or Mesa
dnl    already installed. Tcl and Tk are needed for the Tcl support.
STAR_DECLARE_DEPENDENCIES([build], [mesa tcl tk])

dnl    VTK and CMake do not use any autotools, so most of this is a
dnl    complete fudge. Configuration consists of running bootstrap in CMake
dnl    (this also does a basic build). Make runs the local cmake and then
dnl    make in the VTK directory. Remove cmConfigure.h file as that
dnl    maybe from a full build and include components that are not to be
dnl    used during a bootstrap (cmELF for instance).
(
   cmd="cd cmake && rm -f Source/cmConfigure.h && ./bootstrap";\
   echo $cmd; \
   eval $cmd
)

STAR_SPECIAL_INSTALL_COMMAND([
   ( cd VTK; $(MAKE) DESTDIR=$$DESTDIR install ); \
   ( cd $$DESTDIR$(libdir); \
        for f in `cd vtk-5.10 && ls *$(SHLIB_EXT)*`; do \
           if test -f "$$f"; then rm "$$f"; fi; \
	   mv vtk-5.10/$$f .; \
	done );
   ( cd $$DESTDIR$(prefix)/include && rm -rf vtk && $(LN_S) vtk-5.10 vtk )
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
