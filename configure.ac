dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
AC_INIT(startree, 0.1, ussc@star.rl.ac.uk)
AC_PREREQ(2.50)

AC_CONFIG_SRCDIR(Makefile.dependencies)

dnl    The default prefix is NOT set in this file, but must be
dnl    configured into this file by the ./bootstrap script, which gets
dnl    the correct value for this by running `starconf --show
dnl    STARCONF_DEFAULT_PREFIX', and sed'ding the result into the
dnl    ./configure script which autoconf generates.
AC_PREFIX_DEFAULT(%%PREFIX%%)

AC_PATH_PROG([JAVA], [java], [NOJAVA])
AC_ARG_VAR([JAVA], [Location of java runtime])

AC_PROG_MAKE_SET

dnl    The following list of active directories must be maintained by hand
AC_CONFIG_SUBDIRS(applications libraries)

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
