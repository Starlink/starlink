dnl   Configure script for moggy
dnl   Copyright 2001 Council for the Central Laboratory of
dnl                  the Research Councils. 
dnl   See file LICENCE for conditions.
dnl
dnl   $Id$


AC_REVISION($Revision$)dnl


AC_INIT(moggy, 0.1, [norman@astro.gla.ac.uk])

AM_INIT_AUTOMAKE

dnl   Use a config file (the list of settings is getting too long)
AC_CONFIG_HEADER(config.h)

dnl   Say where the install-sh, config.guess and config.sub scripts are
dnl AC_CONFIG_AUX_DIR(config)

dnl   Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_RANLIB

dnl   Similarly, check for uncompress and gzip programs
AC_PATH_PROG(PROGUNCOMPRESS, uncompress)
AC_PATH_PROG(PROGGZIP, gzip)

dnl   For printing messages...
AC_SUBST(ECHO_C)
AC_SUBST(ECHO_N)
AC_SUBST(ECHO_T)

dnl   Check for a Perl binary, looking in the likely Starlink area
dnl   first.  We need to do more than just AC_PATH_PROG(PERLPATH,
dnl   perl, noperl, /star/Perl/bin:/stardev/Perl/bin:$PATH), since we
dnl   must additionally check that the perl which we find is new
dnl   enough that it can load autoastrom.pm.
dnl
dnl   In fact, Moggy.pm currently _requires_ StarPerl (that is,
dnl   vanilla perl won't work), since it uses autoastrom.pm, which in turn
dnl   loads the NDF library; it only uses deg2sex from autoastrom.pm,
dnl   however, so that if it were desirable to make Moggy independent
dnl   of autoastrom, this function would have to be copied over.  If
dnl   this were done, then the test for Perl could revert to the
dnl   simple one above.
AC_CHECKING(whether we can find a suitable Perl)
starpath="/star/Perl/bin /stardev/Perl/bin"
if test -n "$STARLINK"; then
    starpath="$STARLINK/Perl/bin $starpath"
fi
echo STARPATH=$starpath
for p in $starpath; do
    pp=$p/perl
    if test -x $pp; then
        AC_MSG_CHECKING(whether $pp can load autoastrom)
        if $pp -e 'use lib ".."; use autoastrom; exit 0;'; then
            PERLPATH=$pp
            AC_MSG_RESULT(ok)
            break
        else
            AC_MSG_RESULT(no)
        fi
    fi
done
if test -z "$PERLPATH"; then
    AC_MSG_ERROR([Can't find a suitable Perl in $starpath])
fi
AC_SUBST(PERLPATH)

dnl   Use C++ to compile test programs
AC_LANG([C++])

dnl   Check if we've got sstream.  If not, we'll assume we have the older
dnl   strstream.
AC_CHECK_HEADERS([sstream strstream map])

dnl   Use C for checks after this
AC_LANG_PUSH(C)
AC_CHECK_DECLS([setenv, getenv, unsetenv, putenv])
AC_CHECK_FUNCS([setenv getenv unsetenv putenv])
AC_LANG_POP(C)

dnl   Check whether we need to include `-lnsl -lsocket' to use the
dnl   socket() call and friends (catlib needs these functions, and you
dnl   do need these on Solaris)
AC_CHECK_FUNC(gethostbyname,,
  AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(socket,,
  AC_CHECK_LIB(socket, socket,,,-lnsl))

dnl   Do we have to add any platform-specific compiler flags.  We
dnl   could do this by using AC_CANONICAL_HOST to detect the platform
dnl   and add things that way, but it's rather error-prone, since it
dnl   depends on config.guess being up-to-date, and possibly on
dnl   compilers not changing.  Try compiling with a sequence of added
dnl   CXXFLAGS and go with the first one that works.  The streams
dnl   support is the only one that appears to matter -- others may be
dnl   added here in future.
dnl
dnl   The alpha cxx compiler currently (v6.0) needs to be invoked with
dnl   `-std strict_ansi' (or with __USE_STD_IOSTREAM defined), so that
dnl   it uses a standards-conforming streams library.  See Digital C++
dnl   v6.0 manual, section 7.1.2, at eg,
dnl   <http://h30097.www3.hp.com/cplus/ugu.htm>
dnl
dnl   XXX This _doesn't_ work on cxx V6.5 at least:
dnl   catlib-install/include/cat/CatalogInfo.h includes iostream.h,
dnl   which includes iostream.hxx (the old library interface), which
dnl   has an explicit check for __NO_USE_STD_IOSTREAM (should be
dnl   exclusive with __USE_STD_IOSTREAM) and refuses to load unless
dnl   it's defined.  That's a bigger problem to deal with....
AC_MSG_CHECKING(extra compiler flags for streams support)
orig_CXXFLAGS="$CXXFLAGS"
flagschecked=no
for flags in "" "-std strict_ansi"; do
    CXXFLAGS="$orig_CXXFLAGS $flags"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
                #include <iostream>
                using std::ostream;
            ]])],
            [AC_MSG_RESULT(ok $flags)
             flagschecked=ok
             break])
done
if test $flagschecked = no; then
    AC_MSG_RESULT([unknown!])
    AC_MSG_ERROR([Can't work out how to compile programs with streams support])
fi

dnl   Do we have (standard) <cname> for C libraries, or just <name.h>?
AC_MSG_CHECKING(whether compiler has <cname> instead of just <name.h>)
AC_TRY_COMPILE([#include <cstdio>],,
  AC_DEFINE(HAVE_CSTD_INCLUDE,
            1,
            [Define as 1 if we have (standard) <cname> libraries instead
             of just <name.h>])
  AC_MSG_RESULT(yes),
  AC_MSG_RESULT(no))

dnl   Are the standard functions in the std namespace?
AC_MSG_CHECKING(whether standard functions are in namespace std)
AC_TRY_COMPILE([#include <string>], [string t;],
  AC_MSG_RESULT(no),
  AC_TRY_COMPILE([#include <string>], [using namespace std;string t;],
    AC_DEFINE(STD_IN_STD_NAMESPACE,
              1,
              [Define as 1 if the standard functions are in the std
               namespace (ie, you have to use std:: to get them)])
    AC_MSG_RESULT(yes),
    AC_MSG_RESULT(ARGHH -- error compiling)))

dnl   Find where the SkyCat cat library has been installed.
dnl   This must go _before_ the check for the STARLINK directories:
dnl   since -L$CATLIBDIR/lib must go before -L/star/lib, if we are to
dnl   find our (modified) libcat.a before the standard starlink one.
AC_SUBST(CATLIBDIR)
AC_ARG_WITH(catlib,
[  --with-catlib[=path]    Give non-default location of SkyCat installation dir],
  if test "$withval" = "yes"; then
    # Default place
    CATLIBDIR=../catlib-install
  elif test "$withval" = "no"; then
    CATLIBDIR=NOTHING
  else
    CATLIBDIR=$withval
  fi,
  CATLIBDIR=../catlib-install)
LIBS="$LIBS -L$CATLIBDIR/lib -lcat"
CPPFLAGS="$CPPFLAGS -I$CATLIBDIR/include"


dnl   Add the Starlink include and library paths, taking the location
dnl   of the /star directory from, in order, the argument to --with-starlink, 
dnl   the $STARLINKS variable, the $STARLINK variable, and finally just /star.
dnl   The --with-starlink and $STARLINKS can be a colon-separated list
dnl   of directories.
AC_ARG_WITH(starlink,
[  --with-starlink=path    Give non-default /star (can be colon-sep'd list)],
  if test "$withval" = "yes"; then
    # equivalent to default
    stardirs=
  elif test "$withval" = "no"; then
    AC_MSG_ERROR(You can't build moggy without Starlink)
  else
    stardirs=$withval
  fi,
  stardirs=)
test -n "$stardirs" || stardirs=$STARLINKS
test -n "$stardirs" || stardirs=$STARLINK
test -n "$stardirs" || stardirs=/star
set `echo $stardirs | sed 's/:/ /g'`
while test $# -gt 0; do
  AC_MSG_CHECKING(Starlink directory $1)
  if test -d "$1" -a -d "$1/include" -a -d "$1/lib"; then
    AC_MSG_RESULT(ok)
    CPPFLAGS="$CPPFLAGS -I$1/include"
    LIBS="$LIBS -L$1/lib"
  else
    AC_MSG_RESULT(not found)
    AC_MSG_ERROR(STARLINK directory $1 not found)
  fi
  shift
done

dnl   Find which AST libraries we want to use.  The -I and -L specifiers must
dnl   go at the _beginning_ of the list of libraries, so that the
dnl   given directory is searched before /star
AC_ARG_WITH(ast,
[  --with-ast=path         Give /star-equivalent directory where AST is to be found],
  if test "$withval" = "yes" -o "$withval" = "no"; then
    # same as default
    LIBS="$LIBS `ast_link`"
  else
    CPPFLAGS="-I$withval $CPPFLAGS"
    LIBS="-L$withval `ast_link` $LIBS"
  fi,
  LIBS="$LIBS `ast_link`")
  
dnl   Following tests need the ast_link collection of libraries to be
dnl   specified in LIBS.  This will work, because we've make sure that
dnl   the appropriate -L option in already in LIBS, when we processed
dnl   --with-starlink above.
dnl LIBS="$LIBS `ast_link`"




dnl   Now that we've found the /star (or other AST) directories, check
dnl   that ast.h is there.  There was a problem with ast.h which meant
dnl   it caused the Tru64 C++ compiler to object (this is certainly
dnl   fixed by AST 1.8-1).  This is therefore more direct than the
dnl   obvious alternative of testing for the preprocessor constants
dnl   AST_{MAJOR,MINOR}_VERS.
AC_CHECK_HEADER(ast.h)
AC_MSG_CHECKING(whether your AST headers are usable)
AC_TRY_COMPILE([extern "C" {
#include <ast.h>
}],,
  AC_MSG_RESULT(ok),
  AC_MSG_RESULT(not on this platform)
  AC_MSG_ERROR([[consider defining STARLINK to point to a newer distribution, or give an alternative AST location using --with-ast]]))

dnl   Check that the $LIBS path we now have does work for linking
dnl   against the AST function astOK_.  Note that we do provide a
dnl   non-null action-if-found, here.  The default prepends -last to
dnl   $LIBS, but we want to ensure that that stays safely behind
dnl   -Last-dir (there's no difference for Linux ld, for example, but
dnl   DUX ld has -L options apply only to the -l options which follow them).
AC_CHECK_LIB(ast, astOK_,
  AC_DEFINE(HAVE_LIBAST,
            1,
            [Define as 1 if we do have a functioning AST library]),
  AC_MSG_ERROR([[consider defining STARLINK to point to a newer distribution, or give an alternative AST location using --with-ast]]))

dnl   Do we have the wcslib glsfwd function?  This might be in
dnl   ast_wcslib (if that's very old) or in $CATLIBDIR/lib/libcat.a
dnl   (it might have been _removed_ from the latter by the autoastrom
dnl   makefile, if it is present in ast_wcslib).  If we can't find a
dnl   copy, we supply one ourselves in SkyCatHoles.c -- see the
dnl   discussion of this problem in that file.
AC_LANG_PUSH(C)
AC_REPLACE_FUNCS([glsfwd slaDeuler])
AC_LANG_POP(C)


AC_CONFIG_FILES(Makefile t/Makefile t/runtests)
AC_OUTPUT
