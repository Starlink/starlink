#  Originally from the autoconf macro archive.
#  This code is now a fork so update with care.
#
# SYNOPSIS
#
#   MDL_HAVE_OPENGL
#
# DESCRIPTION
#
#   Search for OpenGL. We search first for Mesa (a GPL'ed version of
#   Mesa) before a vendor's version of OpenGL, unless we were
#   specifically asked not to with `--with-Mesa=no' or
#   `--without-Mesa'.
#
#   The four "standard" OpenGL libraries are searched for: "-lGL",
#   "-lGLU", "-lGLX" (or "-lMesaGL", "-lMesaGLU" as the case may be)
#   and "-lglut".
#
#   All of the libraries that are found (since "-lglut" or "-lGLX"
#   might be missing) are added to the shell output variable "GL_LIBS",
#   along with any other libraries that are necessary to successfully
#   link an OpenGL application (e.g. the X11 libraries). Care has been
#   taken to make sure that all of the libraries in "GL_LIBS" are
#   listed in the proper order.
#
#   Additionally, the shell output variable "GL_CFLAGS" is set to any
#   flags (e.g. "-I" flags) that are necessary to successfully compile
#   an OpenGL application.
#
#   The following shell variable (which are not output variables) are
#   also set to either "yes" or "no" (depending on which libraries were
#   found) to help you determine exactly what was found.
#
#     have_GL
#     have_GLU
#     have_GLX
#     have_glut
#
# LAST MODIFICATION
#
#   2007-07-29
#
# COPYLEFT
#
#   Copyright (c) 2007 Matthew D. Langston
#   Copyright (c) 2007 Ahmet Inan <auto@ainan.org>
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation; either version 2 of the
#   License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
#   02111-1307, USA.
#
#   As a special exception, the respective Autoconf Macro's copyright
#   owner gives unlimited permission to copy, distribute and modify the
#   configure scripts that are the output of Autoconf when processing
#   the Macro. You need not follow the terms of the GNU General Public
#   License when using or distributing such scripts, even though
#   portions of the text of the Macro appear in them. The GNU General
#   Public License (GPL) does govern all other use of the material that
#   constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the
#   Autoconf Macro released by the Autoconf Macro Archive. When you
#   make and distribute a modified version of the Autoconf Macro, you
#   may extend this special exception to the GPL to apply to your
#   modified version as well.
AC_DEFUN([MDL_HAVE_OPENGL],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_CACHE_CHECK([for OpenGL], mdl_cv_have_OpenGL,
  [
dnl Check for Mesa first, unless we were asked not to.
    AC_ARG_ENABLE(Mesa, [  --enable-Mesa           prefer the Mesa library over a vendors native OpenGL library],
                  use_Mesa=$enableval, use_Mesa=yes)
    if test x"$use_Mesa" = xyes; then
       GL_search_list="MesaGL   GL"
      GLU_search_list="MesaGLU GLU"
      GLX_search_list="MesaGLX GLX"
    else
       GL_search_list="GL  MesaGL"
      GLU_search_list="GLU MesaGLU"
      GLX_search_list="GLX MesaGLX"
    fi

    AC_LANG_PUSH([C])

dnl If we are running under X11 then add in the appropriate libraries.
    if test x"$no_x" != xyes; then
dnl    Add everything we need to compile and link X programs to GL_X_CFLAGS
dnl    and GL_X_LIBS.
       GL_CFLAGS="$X_CFLAGS"
       GL_X_LIBS="$X_PRE_LIBS $X_LIBS $X_EXTRA_LIBS"
    fi
    GL_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$GL_CFLAGS"

    GL_save_LIBS="$LIBS"
    LIBS="$GL_X_LIBS"

    # Save the "AC_MSG_RESULT file descriptor" to FD 8.
    exec 8>&AS_MESSAGE_FD

    # Temporarily turn off AC_MSG_RESULT so that the user gets pretty
    # messages.
    exec AS_MESSAGE_FD>/dev/null

    AC_SEARCH_LIBS(glAccum,          $GL_search_list, have_GL=yes,   have_GL=no)
    AC_SEARCH_LIBS(gluBeginCurve,   $GLU_search_list, have_GLU=yes,  have_GLU=no)
    AC_SEARCH_LIBS(glXChooseVisual, $GLX_search_list, have_GLX=yes,  have_GLX=no)
    AC_SEARCH_LIBS(glutInit,        glut,             have_glut=yes, have_glut=no)


    # Restore pretty messages.
    exec AS_MESSAGE_FD>&8

    if test -n "$LIBS"; then
      mdl_cv_have_OpenGL=yes
      GL_LIBS="$LIBS"
    else
      mdl_cv_have_OpenGL=no
      GL_CFLAGS=
    fi

dnl Reset GL_X_LIBS regardless, since it was just a temporary variable
dnl and we don't want to be global namespace polluters.
    GL_X_LIBS=

    LIBS="$GL_save_LIBS"
    CPPFLAGS="$GL_save_CPPFLAGS"

    AC_LANG_POP([C])

dnl bugfix: dont forget to cache this variables, too
    mdl_cv_GL_CFLAGS="$GL_CFLAGS"
    mdl_cv_GL_LIBS="$GL_LIBS"
    mdl_cv_have_GL="$have_GL"
    mdl_cv_have_GLU="$have_GLU"
    mdl_cv_have_GLX="$have_GLX"
    mdl_cv_have_glut="$have_glut"
  ])


  GL_CFLAGS="$mdl_cv_GL_CFLAGS"
  GL_LIBS="$mdl_cv_GL_LIBS"
  have_GL="$mdl_cv_have_GL"
  have_GLU="$mdl_cv_have_GLU"
  have_GLX="$mdl_cv_have_GLX"
  have_glut="$mdl_cv_have_glut"

dnl Do this outside of the cache check. That cannot have side-effects in case
dnl the cache check succeeds and the code is not ran.
  AC_SUBST([GL_CFLAGS])
  AC_SUBST([GL_LIBS])

])
dnl endof bugfix -ainan
