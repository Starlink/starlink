dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(astrom, 3.8, starlink@jiscmail.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR([astrom.f])

STAR_DEFAULTS

AC_PROG_FC
AC_PROG_FPP
AC_FC_OPEN_SPECIFIERS(readonly)

dnl    Use libtool for linking since it will try to use the -rpath option
AC_PROG_LIBTOOL

dnl  Specify the fortran unit numbers for standard input and output.
dnl  We should test for this, but (a) I'm not sure how I'd write such
dnl  a test, and (b) they really are pretty standard, and this is just
dnl  covering my back.
AC_SUBST(FUNITIN, 5)
AC_SUBST(FUNITOUT, 6)

STAR_CNF_COMPATIBLE_SYMBOLS

dnl   Check whether we need to include `-lnsl -lsocket' to use the
dnl   socket() call and friends (the t/makefile needs this, and the
dnl  moggy configuration will use this also)
AC_CHECK_FUNC(gethostbyname,,
  AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(socket,,
  AC_CHECK_LIB(socket, socket,,,-lnsl))

dnl  Default WCS style.
AC_ARG_WITH(wcsstyle,
            AS_HELP_STRING([--with-wcsstyle],
                           [Set default output WCS style]),
            [if test -z "$withval" -o "$withval" = yes; then
                 ASTROMWCSSTYLE=qtan
             elif test "X$withval" = Xno; then
                 AC_MSG_ERROR([--without-wcsstyle makes no sense])
             else
                 ASTROMWCSSTYLE=$withval
             fi],
            ASTROMWCSSTYLE=qtan)
AC_SUBST(ASTROMWCSSTYLE)

STAR_DECLARE_DEPENDENCIES(build, [sla cfitsio])

STAR_LATEX_DOCUMENTATION(sun5)

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES([Makefile params.f t/Makefile component.xml])
AC_OUTPUT
