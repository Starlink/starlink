dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 24276 $)

dnl    Initialisation: package name and version number
AC_INIT(vtk, 6.1.0, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl  Need to work out where the X11 libraries are. On a Mac
dnl  VTK always looks for the OpenGL framework even if building
dnl  against X11. Trap case where no special X11 location is needed.
AC_PATH_XTRA
X11_INCLUDES=$x_includes
X11_LIBS=$x_libraries
AC_SUBST(X11_INCLUDES)
AC_SUBST(X11_LIBS)
AM_CONDITIONAL( NEED_GLPATH, [test "x$x_includes" != "x"])

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
   cmd="cd cmake && rm -f Source/cmConfigure.h && ./bootstrap --no-qt-gui --system-curl";\
   echo $cmd; \
   eval $cmd
)

STAR_SPECIAL_INSTALL_COMMAND([
   ( cd VTK; $(MAKE) DESTDIR=$$DESTDIR install ); \
   ( cd $$DESTDIR$(libdir) && rm -f vtk-6.1 && mv tcltk/vtk-6.1 . ); \
   ( cd $$DESTDIR$(prefix)/include && rm -rf vtk && $(LN_S) vtk-6.1 vtk )
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
