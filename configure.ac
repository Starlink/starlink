dnl    Process this file with autoconf to produce a configure script

dnl    This `package name' is a dummy
AC_INIT(dummy-startree, 1.0, ussc@star.rl.ac.uk)
AC_CONFIG_AUX_DIR(config)
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
AC_PROG_LN_S

dnl    The following list of active directories must be maintained by hand
dnl    Leave out thirdparty for the moment -- the only directories
dnl    there are fsf (which contains buildsupport tools which are
dnl    configured as part of the bootstrap process) and eso (which
dnl    contains libraries which haven't yet been brought within the
dnl    Starlink build system).
AC_CONFIG_SUBDIRS(applications libraries buildsupport)
dnl AC_CONFIG_SUBDIRS(applications libraries buildsupport thirdparty)

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
