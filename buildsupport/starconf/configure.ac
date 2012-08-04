dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(starconf, 1.3, starlink@jiscmail.ac.uk)
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
AC_PREFIX_DEFAULT(/star/buildsupport)


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


dnl   Specify the default installation prefix of components configured
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
if expr $STARCONF_DEFAULT_PREFIX : '.*/$' >/dev/null; then
    echo "STARCONF_DEFAULT_PREFIX=$STARCONF_DEFAULT_PREFIX has trailing slash" >&2
    exit 1
fi
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
AC_PATH_PROG(ACLOCAL, aclocal, unknown)
AC_PROG_AWK

AC_PATH_PROG([JAVAC], [javac], [NOJAVAC])
AC_ARG_VAR([JAVAC], [Location of java compiler])
AC_PATH_PROG([JAVA], [java], [NOJAVA])
AC_ARG_VAR([JAVA], [Location of java runtime])
AC_PATH_PROG([JAR], [jar], [NOJAR])


# STAR_CHECK_JAVAC([version])
# ---------------------------
# Check that $JAVAC can compile Java code.  If VERSION is present, it must
# be of the form 1.x, and the $JAVAC is checked to compile that version
# of Java.  If not present, check 1.4.
## (1) Yes, this is overkill for this problem, but (a) it might be useful
## for the java/ part of the tree, and (b) it's not much more hassle to do
## it properly than to hack it up.
## (2) The ac_ext, _AC_RUN_LOG and _AC_MSG_LOG_CONFTEST below are all
## internal to autoconf.
AC_DEFUN([STAR_CHECK_JAVAC],
 [if test "$JAVAC" = NOJAVA; then
    AC_MSG_NOTICE([No Java compiler found -- skipping version check])
  else
    m4_ifval([$1],
             [m4_define([tmp_v],m4_bregexp([$1], [^1\.\([0-9]\)$], \1))dnl
              jdkversion=m4_ifval(tmp_v,tmp_v,
                                  [AC_FATAL([$0: malformed version $1])])
              m4_undefine([tmp_v])],
             [jdkversion=4])
    ac_ext=java
    AC_MSG_CHECKING([whether ${JAVAC} can compile Java 1.$jdkversion code])
    rm -f conftest.*
    exec 8>conftest.java
    test $jdkversion -ge 2 && echo "import java.util.Map;"           >&8
    test $jdkversion -ge 3 && echo "import java.util.Timer;"         >&8
    test $jdkversion -ge 4 && echo "import java.util.regex.Pattern;" >&8
    echo "public class conftest { public static void main(String args[]) {" >&8
    test $jdkversion -ge 4 && echo "assert(true);"                   >&8
    echo "System.exit(0); } }"                                       >&8
    exec 8>&-

    if test $jdkversion -ge 4; then
        trycmds="$JAVAC -source 1.4|$JAVAC"
    else
        trycmds="$JAVAC"
    fi
    JAVAC=NOJAVAC
    as_save_IFS=$IFS; IFS=\|
    for cmd in $trycmds
    do
        IFS=$as_save_IFS
        if _AC_RUN_LOG([$cmd conftest.java], [echo Testing $cmd]); then
            JAVAC="$cmd"
            break
        else
            _AC_MSG_LOG_CONFTEST
        fi
    done
    if test "$JAVAC" = NOJAVAC; then
        AC_MSG_RESULT(no)
    else
        AC_MSG_RESULT(yes)
    fi
    rm -f conftest.*
  fi
])# STAR_CHECK_JAVAC

STAR_CHECK_JAVAC(1.4)

AM_CONDITIONAL(BUILDJAVA, test "$JAVAC" != NOJAVAC -a "$JAR" != "NOJAR")


# We install material in the aclocal macro directory with aclocal_DATA
# (as recommended in the automake manual).  However (slightly
# unexpectedly) this won't work unless we also define aclocaldir --
# it's not predefined.  This is probably good, as it ensures the
# aclocaldir in question is the one reported by the aclocal in the path.
if test "$ACLOCAL" = unknown; then
    AC_MSG_ERROR([Can't find aclocal -- have the autotools been installed?])
fi
aclocaldir=`$ACLOCAL --print-ac-dir`
AC_SUBST(aclocaldir)


# Convert the version number in PACKAGE_VERSION into an integer
# Pick a version (I'm getting bored with this project...)
PACKAGE_VERSIONINT=[`echo $PACKAGE_VERSION | sed 's/[^0-9]/ /g' | $AWK '{print $1*1000000+$2*1000+$3}'`]
AC_SUBST(PACKAGE_VERSIONINT)


dnl   Note that the make-starconf.in file is _not_ to be
dnl   substituted by this command -- since it involves the
dnl   installation directory variables, it should have them replaced
dnl   at `make' time, not configure time.  See Makefile.am for details.
AC_CONFIG_FILES(Makefile starconf.m4 bootstrap.installed component.xml)
AC_CONFIG_FILES(java/Makefile)
AC_CONFIG_FILES(starconf-finder, [chmod +x starconf-finder])
AC_CONFIG_FILES(starconf-validate, [chmod +x starconf-validate])
AC_CONFIG_FILES(starconf-deinstall, [chmod +x starconf-deinstall])


AC_OUTPUT
