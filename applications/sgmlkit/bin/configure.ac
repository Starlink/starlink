dnl    Process this file with autoconf to produce a configure script

AC_INIT(ssn70-bin)

AC_CONFIG_SRCDIR(sgml2docs.in)

AC_ARG_VAR(STARLINK_SGML_DIR)
test -n "$STARLINK_SGML_DIR" || STARLINK_SGML_DIR=`cd ..; pwd`

AC_PATH_PROGS(JADE, [openjade jade], jade)
AC_PATH_PROGS(SGMLNORM, [osgmlnorm sgmlnorm], sgmlnorm)
AC_PATH_PROG(DVI2BITMAP, dvi2bitmap, dvi2bitmap)

AC_CONFIG_FILES(sgml2docs sgml2hlp)
AC_OUTPUT



