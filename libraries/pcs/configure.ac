dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(pcs, 4.1-1, ussc@star.rl.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR([compifl.f])


STAR_DEFAULTS


AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
AC_PROG_LN_S

AC_FC_LIBRARY_LDFLAGS
STAR_CNF_COMPATIBLE_SYMBOLS


AC_PATH_PROGS(M4, m4, m4)

AC_FC_CHECK_INTRINSICS([getcwd chdir putenv])
AC_FC_OPEN_SPECIFIERS([readonly])

dnl    On OSX you need to be root to run mknod
AC_CHECK_FUNCS([atexit on_exit mkfifo mknod])
dnl    Extra libraries needed to link against sockets on this system.
dnl    These tests are definitely needed for Solaris.
AC_CHECK_FUNC(gethostbyname,,
  AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(socket,,
  AC_CHECK_LIB(socket, connect,,,-lnsl))

AC_CHECK_HEADERS([curses.h cursesX.h term.h ncurses/term.h])


dnl    Because we build the application compifl, we need to link against
dnl    the following components, rather than just use their include files,
dnl    as would be the case for a purely-libraries component.
dnl    (mers is required, since dtask/Makefile.am invokes dtask_link_adam,
dnl    which invokes err_link_adam in turn)
STAR_DECLARE_DEPENDENCIES(build, [cnf ems hds hlp mers psx sae])
STAR_DECLARE_DEPENDENCIES(build, [chr], link)
STAR_DECLARE_DEPENDENCIES(link,  [chr ems hds hlp mers psx])


dnl    All the documentation is in pcs-docs


dnl    There are numerous messgen files, and all should be listed here, so
dnl    that they are listed as `predist' files.
STAR_MESSGEN([atimer/atimer_err.msg
              dtask/act_err.msg dtask/dtask_err.msg lex/lex_err.msg
              messys/messys_err.msg msp/msp_err.msg parsecon/parse_err.msg
              sock/sock_err.msg subpar/subpar_err.msg task/task_err.msg])


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


AC_OUTPUT
