dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(starconf, 1.0-5, ussc@star.rl.ac.uk)
AM_INIT_AUTOMAKE

AC_CONFIG_SRCDIR([starconf.m4.in])

dnl   The default installation location for this package is
dnl   /star/buildsupport.  This is not the default location of
dnl   components configured using this tool and the acinclude.m4 which
dnl   it generates.  That location is specified below by the value of
dnl   STARCONF_DEFAULT_STARLINK, which defaults to the value of
dnl   STARLINK which is current at configuration time.
dnl
dnl   Note that the default for _this_ component does _not_ depend on
dnl   the STARLINK variable (because of the way that m4/autoconf moves
dnl   text around, it's generally unreliable to have variables in this
dnl   macro).  If you wish to relocate this starconf package to match
dnl   STARLINK, do so with --prefix=$STARLINK/buildsupport
AC_PREFIX_DEFAULT(/star)
dnl   ...but put the share files into a non-standard directory, along with the
dnl   other buildsupport tools
datadir=`echo $datadir | sed s,share,buildsupport/share,`


dnl   Specify the default location of a currently installed Starlink
dnl   tree.  This is where we will look for any Starlink tools we need
dnl   to use while we are building the software.  This can be
dnl   specified either by giving DEFAULT_STARLINK=xxx as a parameter to
dnl   the ./configure command, or by letting it be defaulted from the
dnl   current value of $DEFAULT_STARLINK, $STARLINK or "/star"
dnl   (whichever is non-empty)
dnl
dnl   See starconf.m4.in for more discussion on these variables
AC_ARG_VAR(STARCONF_DEFAULT_STARLINK,
           [Default location of a currently installed Starlink tree])
AC_MSG_CHECKING([for a defined or defaulted STARCONF_DEFAULT_STARLINK])
for s in "$STARCONF_DEFAULT_STARLINK" "$STARLINK" /star
do
    test -n "$s" && STARCONF_DEFAULT_STARLINK=$s && break
done
AC_MSG_RESULT($STARCONF_DEFAULT_STARLINK)


dnl   Specify the default intallation prefix of components configured
dnl   by this tool.  This can be specified either by giving
dnl   STARCONF_DEFAULT_PREFIX=xxx as a parameter to the ./configure
dnl   command, or by letting it be defaulted from the current value of
dnl   STARCONF_DEFAULT_PREFIX, STARCONF_DEFAULT_STARLINK,
dnl   STARLINK or "/star".  This need not be the same as the
dnl   definition of AC_PREFIX_DEFAULT above.
AC_ARG_VAR(STARCONF_DEFAULT_PREFIX,
           [Default installation directory of components])
AC_MSG_CHECKING([for a defined or defaulted STARCONF_DEFAULT_PREFIX])
for s in "$STARCONF_DEFAULT_PREFIX" "$STARCONF_DEFAULT_STARLINK" "$STARLINK" /star
do
    test -n "$s" && STARCONF_DEFAULT_PREFIX="$s" && break
done
AC_MSG_RESULT($STARCONF_DEFAULT_PREFIX)


dnl   We do not use macro STAR_DEFAULTS, since it may not exist
dnl   at the time we configure this.


# Set the STAR_MANIFEST_DIR variable, defaulting it from the environment.
# This causes automake to include the `install-manifest' target in Makefile.in
: ${STAR_MANIFEST_DIR='$(prefix)/manifests'}
AC_SUBST(STAR_MANIFEST_DIR)


dnl   Find working tools
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(PERL, perl)

AC_PATH_PROG([JAVAC], [javac], [NOJAVAC])
AC_ARG_VAR([JAVAC], [Location of java compiler])


# Convert the version number in PACKAGE_VERSION into an integer
PACKAGE_VERSIONINT=[`echo $PACKAGE_VERSION | sed 's/[^0-9][^0-9]*/ /g' | awk '{print $1*1000000+$2*1000+(NF>2 ? $3 : 0)}'`]
AC_SUBST(PACKAGE_VERSIONINT)


dnl   Note that the make-starconf.in file is _not_ to be
dnl   substituted by this command -- since it involves the
dnl   installation directory variables, it should have them replaced
dnl   at `make' time, not configure time.  See Makefile.am for details.
AC_CONFIG_FILES(Makefile starconf.m4 bootstrap.installed)
AC_CONFIG_FILES(java/Makefile)
AC_CONFIG_FILES(starconf-finder, [chmod +x starconf-finder])
AC_CONFIG_FILES(starconf-validate, [chmod +x starconf-validate])


AC_OUTPUT
