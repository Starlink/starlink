dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
AC_INIT(star, 0.1, ussc@star.rl.ac.uk)
AC_PREREQ(2.50)

AC_CONFIG_SRCDIR(Makefile.dependencies)

AC_PATH_PROG([JAVA], [java], [NOJAVA])
AC_ARG_VAR([JAVA], [Location of java runtime])

dnl    The following list of active directories must be maintained by hand
AC_CONFIG_SUBDIRS(applications libraries)

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
