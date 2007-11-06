dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 16442 $)

AC_INIT(mesa, 7.0.1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_MAKE_SET

dnl    Check for OpenGL libraries, if these are present and we've
dnl    not been told to proceed regardless, then this component
dnl    will not build anything (its purpose is to provide 3D
dnl    software rendering on platforms without hardware support.
dnl    A remaining question is how to provide this as a option
dnl    regardless, for those machines whose platforms have OpenGL
dnl    but specific hardware doesn't).
sinclude(mdl_have_opengl.m4)
MDL_HAVE_OPENGL
if test "$have_GL" = "yes"; then
   MAIN_TARGET="mesa-stamp"
else
   MAIN_TARGET="mesa-src"
fi
AC_SUBST(MAIN_TARGET)

dnl    MESA has a funny build system, need to determine a platform.
dnl    AC_CANONICAL_BUILD, not needed, but used for build value.
case "$build" in
  *alpha*)                 MESA_SYSTEM=osf1;;
  *solaris*)               MESA_SYSTEM=sunos5-v8;;
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
