dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 16442 $)

AC_INIT([mesa],[7.0.1],[starlink@jiscmail.ac.uk])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.68])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_MAKE_SET
AC_PATH_XTRA

_star_build_vtk=:
AC_ARG_WITH(vtk,
   AS_HELP_STRING(
      [--without-vtk],
      [Do not build VTK and associated software (default --with)]),
   [if test "X$withval" = "Xno"; then
       _star_build_vtk=false
    fi])

dnl    Do not check for an existing OpenGL if requested.
AC_ARG_ENABLE([opengl-check], [  --enable-opengl-check    check for existing OpenGL],
              [opengl_check=$enableval], [opengl_check=yes])

if $_star_build_vtk; then
   dnl    Check for OpenGL libraries, if these are present and we've
   dnl    not been told to proceed regardless, then this component
   dnl    will not build anything (its purpose is to provide 3D
   dnl    software rendering on platforms without hardware support).
   if test "$opengl_check" = "yes"; then
      sinclude(mdl_have_opengl.m4)
      MDL_HAVE_OPENGL
   else
      have_GL=no
      AC_MSG_RESULT([not checking for OpenGL])
   fi
   if test "$have_GL" = "yes"; then
      MAIN_TARGET="mesa-stamp"
   else
      MAIN_TARGET="mesa-src"
   fi
else
   AC_MSG_RESULT([not checking for OpenGL (VTK support disabled)])
   MAIN_TARGET="mesa-stamp"
fi
AC_SUBST(MAIN_TARGET)

dnl    MESA has a funny build system, need to determine a platform.
dnl    AC_CANONICAL_BUILD, not needed, but used for build value.
case "$build" in
  *alpha*)                 MESA_SYSTEM=osf1;;
  *solaris*)               MESA_SYSTEM=sunos5;;
  *darwin*)                MESA_SYSTEM=darwin;;
  *)                       MESA_SYSTEM=linux;;
esac
AC_SUBST(MESA_SYSTEM)

dnl    Use a special install so we can keep the manifest correct.
STAR_SPECIAL_INSTALL_COMMAND([
    if test $(MAIN_TARGET) = "mesa-src"; then
       cd mesa;
       $(MAKE) DESTDIR=$$DESTDIR install;
    fi])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
