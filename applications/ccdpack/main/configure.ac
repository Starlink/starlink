AC_INIT(ccdmain, 4.0-16, ccdpack@starlink.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR(ccd1_cmult.f)
STAR_DEFAULTS
AC_PROG_FC
AC_PROG_CC
AC_PROG_LIBTOOL
STAR_CNF_COMPATIBLE_SYMBOLS

TCL_VERSION=8.2
ITCL_VERSION=3.1
IWIDGETS_VERSION=3.0.0

# Check availability of [incr Tcl].  This is declared as a build dependency,
# but we'll get along without it if it's not there because it couldn't
# be built.
have_itcl=1
AC_CHECK_LIB(itcl$ITCL_VERSION,Itcl_Init,,[unset have_itcl])
AC_CHECK_LIB(itk$ITCL_VERSION,Itk_Init,,[unset have_itcl])
AC_MSG_CHECKING([whether to use incr tcl])
if test -n "$have_itcl"
then
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_ITCL, 1, [Is incr Tcl present?])
else
    AC_MSG_RESULT(no)
fi

AC_CONFIG_FILES(Makefile)
AC_CONFIG_HEADERS(config.h)
AC_OUTPUT
