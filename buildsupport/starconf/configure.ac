dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(starconf, 1.0-2, ussc@star.rl.ac.uk)
AM_INIT_AUTOMAKE

AC_CONFIG_SRCDIR([starconf.m4.in])

dnl   The default installation location for this package is
dnl   /star/buildsupport.  This is not the default location of
dnl   components configured using this tool and the acinclude.m4 which
dnl   it generates.  That location is specified below by the value of
dnl   DEFAULT_STARLINK, which defaults to the value of STARLINK which is
dnl   current at configuration time.
dnl
dnl   Note that the default for _this_ component does _not_ depend on
dnl   the STARLINK variable (because of the way that m4/autoconf moves
dnl   text around, it's generally unreliable to have variables in this
dnl   macro).  If you wish to relocate this starconf package to match
dnl   STARLINK, do so with --prefix=$STARLINK/buildsupport
AC_PREFIX_DEFAULT(/star/buildsupport)

dnl   Specify the default location of a currently installed Starlink
dnl   tree.  This is where we will look for any Starlink tools we need
dnl   to use while we are building the software.  This can be
dnl   specified either by giving DEFAULT_STARLINK=xxx as a parameter to
dnl   the ./configure command, or by letting it be defaulted from the
dnl   current value of $DEFAULT_STARLINK, $STARLINK or "/star"
dnl   (whichever is non-empty)
AC_ARG_VAR(DEFAULT_STARLINK,
           [Default location of a currently installed Starlink tree])
AC_MSG_CHECKING([for a defined or defaulted DEFAULT_STARLINK])
for s in "$DEFAULT_STARLINK" "$STARLINK" /star
do
    test -n "$s" && DEFAULT_STARLINK=$s && break
done
AC_MSG_RESULT($DEFAULT_STARLINK)

dnl   Specify the default intallation prefix of components configured
dnl   by this tool.  This can be specified either by giving
dnl   DEFAULT_PREFIX=xxx as a parameter to the ./configure command, or
dnl   by letting it be defaulted from the current value of
dnl   DEFAULT_PREFIX, DEFAULT_STARLINK, STARLINK or "/star".  This
dnl   need not be the same as the definition of AC_PREFIX_DEFAULT
dnl   above.
AC_ARG_VAR(DEFAULT_PREFIX,
           [Default installation directory of components])
AC_MSG_CHECKING([for a defined or defaulted DEFAULT_PREFIX])
for s in "$DEFAULT_PREFIX" "$DEFAULT_STARLINK" "$STARLINK" /star
do
    test -n "$s" && DEFAULT_PREFIX="$s" && break
done
AC_MSG_RESULT($DEFAULT_PREFIX)

dnl   Find working tools
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(PERL, perl)

PACKAGE_VERSIONINT=[`echo $PACKAGE_VERSION | sed 's/[^0-9][^0-9]*/ /g' | awk '{print $1*1000000+$2*1000+(NF>2 ? $3 : 0)}'`]
AC_SUBST(PACKAGE_VERSIONINT)

dnl   Note that the make-starconf.in file is _not_ to be
dnl   substituted by this command -- since it involves the
dnl   installation directory variables, it should have them replaced
dnl   at `make' time, not configure time.  See Makefile.am for details.
AC_CONFIG_FILES(Makefile starconf.m4 Makefile.starconf bootstrap.installed)
AC_CONFIG_FILES(starconf-validate, [chmod +x starconf-validate])

AC_OUTPUT
