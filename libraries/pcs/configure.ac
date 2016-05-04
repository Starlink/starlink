dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT([pcs],[4.2-0],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR([compifl.f])


STAR_DEFAULTS


AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
LT_INIT
AC_PROG_LN_S

AC_FC_LIBRARY_LDFLAGS
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_CHECK_PROGS([fgeneric])

dnl    Prefer GNU m4, but try with what we have.
AC_PATH_PROGS(M4, gm4 gnum4 m4, m4)

dnl    On OSX you need to be root to run mknod
AC_CHECK_FUNCS([atexit on_exit mkfifo mknod])
dnl    Extra libraries needed to link against sockets on this system.
dnl    These tests are definitely needed for Solaris.
AC_CHECK_FUNC(gethostbyname,,
  AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(socket,,
  AC_CHECK_LIB(socket, connect,,,-lnsl))

dnl    Look for the term interface
dnl    Curses header files are included in the MP_WITH_CURSES macro
AC_CHECK_HEADERS([cursesX.h term.h ncurses/term.h])

AC_CHECK_HEADERS([sys/socket.h])
AC_CHECK_TYPES([socklen_t],[],[],[#include <sys/types.h>
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif])

dnl    We need to find out whether we are looking for libcurses
dnl    or libncurses. On SUSe libcurses is provided by the termcap
dnl    RPM but we really want to use libncurses (ncurses-devel)
dnl    The MP_WITH_CURSES macro comes from the Gnu autoconf macro
dnl    archive. http://autoconf-archive.cryp.to/mp_with_curses.html

AC_DEFUN([MP_WITH_CURSES],
  [AC_ARG_WITH(ncurses, [  --with-ncurses          Force the use of ncurses over curses],,)
   mp_save_LIBS="$LIBS"
   CURSES_LIB=""
   if test "$with_ncurses" != yes
   then
     AC_CACHE_CHECK([for working curses], mp_cv_curses,
       [LIBS="$LIBS -lcurses -ltinfo"
        AC_LINK_IFELSE(
          [AC_LANG_PROGRAM([[#include <curses.h>]],
             [[chtype a; int b=A_STANDOUT, c=KEY_LEFT; initscr(); ]])],
          [mp_cv_curses=yes],[mp_cv_curses=no])])
     if test "$mp_cv_curses" = yes
     then
       AC_DEFINE(HAVE_CURSES_H, 1, [Define to 1 if you have the <curses.h> header file available])
       CURSES_LIB="-lcurses -ltinfo"
     fi
   fi
   if test ! "$CURSES_LIB"
   then
     AC_CACHE_CHECK([for working ncurses], mp_cv_ncurses,
       [LIBS="$mp_save_LIBS -lncurses"
        AC_LINK_IFELSE(
          [AC_LANG_PROGRAM([[#include <ncurses.h>]],
            [[chtype a; int b=A_STANDOUT, c=KEY_LEFT; initscr(); ]])],
          [mp_cv_ncurses=yes],[mp_cv_ncurses=no])])
     if test "$mp_cv_ncurses" = yes
     then
       AC_DEFINE(HAVE_NCURSES_H, 1, [Define to 1 if you have the <ncurses.h> header file available])
       CURSES_LIB="-lncurses"
     fi
   fi
   LIBS="$mp_save_LIBS"
   AC_SUBST(CURSES_LIB)
])dnl

MP_WITH_CURSES

dnl    Because we build the application compifl, we need to link against
dnl    the following components, rather than just use their include files,
dnl    as would be the case for a purely-libraries component.
dnl    The mers dependency is so that dtask_link_adam can call
dnl    err_link_adam at build time
STAR_DECLARE_DEPENDENCIES(build, [cnf ems hds hlp mers psx sae])
STAR_DECLARE_DEPENDENCIES(build, [chr], link)
STAR_DECLARE_DEPENDENCIES(link,  [chr ems hds hlp mers psx])

dnl    Look for fortran intrinsics

# Use Fortran for the remaining function tests
AC_LANG_PUSH([Fortran])

AC_FC_CHECK_INTRINSICS([getarg get_command_argument iargc command_argument_count])
AC_CHECK_FUNCS([getarg get_command_argument iargc command_argument_count])

AC_FC_CHECK_INTRINSICS([getcwd chdir])
AC_FC_OPEN_SPECIFIERS([readonly])
AC_FC_OPEN_SPECIFIERS([action='read'])

AC_LANG_POP([Fortran])

dnl    There are numerous messgen files, and all should be listed here, so
dnl    that they are listed as `predist' files.
STAR_MESSGEN([atimer/atimer_err.msg
              dtask/act_err.msg dtask/dtask_err.msg lex/lex_err.msg
              messys/messys_err.msg msp/msp_err.msg parsecon/parse_err.msg
              sock/sock_err.msg subpar/subpar_err.msg task/task_err.msg])


dnl    Conditional defining whether we build with POSIX thread support.
dnl    We do this by default.
AC_ARG_WITH([pthreads],
            [ --with-pthreads   Build package with POSIX threads support],
            [if test "$withval" = "yes"; then
               use_pthreads="yes"
             else
               use_pthreads="no"
             fi],
            [use_pthreads="yes"])
if test "$use_pthreads" = "yes"; then
   AC_CHECK_LIB([pthread], [pthread_create], ,[use_pthreads="no"])
   if test "$use_pthreads" = "yes"; then
      AC_DEFINE([USE_PTHREADS], [1], [Build with POSIX threads support])
   fi
fi

dnl    The documentation is kept in a separate directory, and configured
dnl    separately.  The Makefile.am does not use the @subdirs@ variable.
AC_CONFIG_SUBDIRS(pcs-docs)


AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile component.xml
                adam/Makefile ams/Makefile atimer/Makefile dtask/Makefile
                hdspar/Makefile lex/Makefile messys/Makefile misc/Makefile
                msp/Makefile parsecon/Makefile sock/Makefile string/Makefile
                subpar/Makefile task/Makefile])
AC_CONFIG_FILES([sock/sock_link],      [chmod 755 sock/sock_link])
AC_CONFIG_FILES([sock/sock_link_adam], [chmod 755 sock/sock_link_adam])
AC_CONFIG_FILES([subpar/subpar_link_adam], [chmod 755 subpar/subpar_link_adam])


AC_OUTPUT
