dnl    Process this file with autoconf to produce a configure script

dnl    This `package name' is a dummy
AC_INIT(startree, 1.0, ussc@star.rl.ac.uk)
AC_PREREQ(2.50)

AC_CONFIG_SRCDIR(Makefile.dependencies)

dnl    The default prefix is NOT set in this file, but must be
dnl    configured into this file by the ./bootstrap script, which gets
dnl    the correct value for this by running `starconf --show
dnl    STARCONF_DEFAULT_PREFIX', and sed'ding the result into the
dnl    ./configure script which autoconf generates.
AC_PREFIX_DEFAULT(%%PREFIX%%)
if test $ac_default_prefix = %%'PREFIX'%%; then
    echo "You have run recently autoconf in the top-level build directory."
    echo "This won't work: either run ./bootstrap instead,"
    echo "or search for %%"PREFIX"%% in ./bootstrap to see how to"
    echo "fix this up."
    exit 1
fi

dnl    If no java can be found, set this to the magic (and obviously failed)
dnl    value NOJAVA.  This is tested for in Makefile.in
AC_PATH_PROG([JAVA], [java], [NOJAVA])
AC_ARG_VAR([JAVA], [Location of java runtime])

AC_PROG_MAKE_SET
AC_PROG_LN_S

dnl    The following list of active directories must be maintained by hand
dnl
dnl    Do NOT mention buildsupport in AC_CONFIG_SUBDIRS.  If we
dnl    include buildsupport here, then ./configure will recurse into
dnl    buildsupport/starconf, which means we end up reconfiguring it
dnl    with a wrong --prefix whenever we ./configure in the top-level
dnl    directory.
AC_CONFIG_SUBDIRS(applications libraries etc thirdparty)

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
