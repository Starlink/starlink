dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(scb, 1.1-3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([tag.c])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_RANLIB
AC_PATH_PROG(PERL, perl)

dnl    If you write AC_PROG_YACC here, autoconf may try to use bison -y.
dnl    This doesn't seem to work for some reason, but yacc does, so get
dnl    it by hand. SCB also prefers byacc on Cygwin.
AC_PATH_PROGS(YACC, [byacc yacc])

dnl    Locate lex, and customise it to use a local skel flag if it's flex.
dnl    If not, audibly deprecate for distribution, since lex-generated
dnl    C may not be portable.
AM_PROG_LEX
if test $LEX = "flex"
then
   LEX="$LEX -Sflex.skl"
else
   AC_MSG_WARN([Do NOT distribute source code created by the "$LEX$" command.])
   AC_MSG_WARN([Only code produced by the flex command is recommended])
   AC_MSG_WARN([for distribution.])
fi

dnl    Set up with defaults some variables which configure where files are
dnl    found.
SCB_INDEX=$prefix/etc/scb
SCB_SOURCES=/stardev/sources
SCB_BROWSER_TMP=/usr/tmp/scb
SCB_INDEXER_TMP=/usr/tmp/scbindexer
HTX_SERVER=http://star-www.rl.ac.uk/cgi-bin/htxserver/

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun225)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES(scb_test.sh, [chmod +x scb_test.sh])

dnl    This is the bit that does the actual work
AC_OUTPUT
