dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(scb, 1.1-3, starlink@jiscmail.ac.uk)

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
STAR_CHECK_PROGS([perl])

dnl    If you write AC_PROG_YACC here, autoconf may try to use bison -y.
dnl    This doesn't seem to work for some reason, but yacc does, so get
dnl    it by hand. SCB also prefers byacc on Cygwin. Failure to locate
dnl    one of these stops the configure.
AC_PATH_PROGS(YACC, [byacc yacc], :)
if test "$YACC" = ":"; then
  AC_MSG_ERROR([Failed to locate yacc or byacc])
fi

dnl    Locate lex, and customise it to use a local skel flag if it's flex.
dnl    Check that we can in fact use the local skel file (this doesn't
dnl    work with versions of flex much after 2.5.4), and fall back to
dnl    plain "flex" if not.  This will cause a slight decrease in scb's
dnl    performance.
dnl
dnl    If flex is not available, and we have to use plain lex instead,
dnl    warn against distributing any generates source code, since
dnl    lex-generated C may not be portable. 
dnl    Not having a lex is serious, so stop (don't use AM_PROG_LEX test
dnl    that uses the missing script when lex isn't found).
AC_PROG_LEX
if test "$LEX" = ":"; then
  AC_MSG_ERROR([Failed to locate lex or flex])
fi

if test $LEX = "flex"
then
   for cmd in "$LEX -Sflex.skl" "$LEX" lex FAIL
   do
      echo "%%" | $cmd >/dev/null 2>/dev/null && break
   done
   rm -f lex.yy.c
   if test "$cmd" = FAIL
   then
      AC_MSG_ERROR([Unable to run lex ($LEX)])
   else
      LEX=$cmd
   fi
fi
dnl Did we start, or have we ended up, with a non-flex lexer?
if expr "$LEX" : flex >/dev/null
then
   : OK
else
   AC_MSG_WARN([lex command is: $LEX])
   AC_MSG_WARN([Do NOT distribute source code created by this command.])
   AC_MSG_WARN([Only code produced by the flex command is recommended])
   AC_MSG_WARN([for distribution.])
fi

dnl    Set up with defaults some variables which configure where files are
dnl    found.
SCB_INDEX=$prefix/etc/scb
SCB_SOURCES=/stardev/sources
SCB_BROWSER_TMP=/usr/tmp/scb
SCB_INDEXER_TMP=/usr/tmp/scbindexer
HTX_SERVER=http://www.starlink.ac.uk/cgi-bin/htxserver/

AC_SUBST(SCB_INDEX)
AC_SUBST(SCB_SOURCES)
AC_SUBST(SCB_BROWSER_TMP)
AC_SUBST(SCB_INDEXER_TMP)
AC_SUBST(HTX_SERVER)

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun225)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES(scb_test.sh, [chmod +x scb_test.sh])
AC_CONFIG_FILES(size_test.sh, [chmod +x size_test.sh])

dnl    This is the bit that does the actual work
AC_OUTPUT
