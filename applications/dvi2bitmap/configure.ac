dnl
dnl   Configure script for dvi2bitmap
dnl
dnl   Copyright 1999--2003, Council of the Central Laboratory
dnl       of the Research Councils
dnl   See file LICENCE for conditions.


AC_INIT(dvi2bitmap, 0.11b1, norman@astro.gla.ac.uk)
AM_INIT_AUTOMAKE
AC_REVISION($Revision$)

dnl   The syntax in here is for autoconf 2.50 and later
AC_PREREQ(2.50)

AC_CONFIG_SRCDIR([dvi2bitmap.cc])

dnl   Say where the install-sh, config.guess and config.sub scripts
dnl   are.
dnl
dnl   Hmm, something appears to go wrong with this: autoconf
dnl   2.57 appears to generate a ./configure which doesn't look in
dnl   the specified directory.  Also, some automake list messages,
dnl   including the thread that contains
dnl   <http://mail.gnu.org/archive/html/automake/2002-09/msg00167.html>
dnl   suggest that libtool 1.4.3 fails to work with configure.ac
dnl   scripts that specify AC_CONFIG_AUX_DIR.  Ugly though it is,
dnl   perhaps it's worth just having all the configure junk in the
dnl   top-level directory.  Perhaps I should join the automake list.
dnl AC_CONFIG_AUX_DIR(config)

dnl   Use a config file (the list of settings is getting too long)
AC_CONFIG_HEADER(config.h)

dnl   Automake sees AC_CONFIG_HEADER as a licence to invoke autoheader
dnl   to make config.h.in from this file.  This is silly (I think,
dnl   because (a) I already have a perfectly good config.h.in, and
dnl   (b) it's carefully ordered and documented, in a way that the
dnl   generated config.h.in isn't.  So disable that step, following
dnl   <http://mail.gnu.org/archive/html/automake/2003-01/msg00051.html>
AUTOHEADER="echo autoheader ignored"


dnl   Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_LIBTOOL

dnl  Use C to compile the AC_CHECK_FUNCS tests (using C++ can cause
dnl  spurious linking errors on, for example, Solaris)
AC_LANG([C])

AC_PATH_PROG(PERL, perl)

dnl   Checks for header files.
AC_CHECK_HEADERS(assert.h unistd.h fcntl.h)

dnl   I don't use HAVE_STDLIB_H, but kpathsea does
AC_CHECK_HEADERS(stdlib.h)

dnl   Check if we've got sstream.  If not, we'll assume we have the older
dnl   strstream
AC_CHECK_HEADERS(sstream)

dnl   Checks for library functions
AC_CHECK_FUNCS(pipe snprintf setenv putenv)
dnl   getopt_long.c uses (and provides a replacement for)
dnl   this glibc utility function
AC_CHECK_FUNCS(__strchrnul)

dnl   We ought not to have to worry about getopt.h, getopt{,1}.c,
dnl   since they can always be unconditionally compiled and linked.  See
dnl   http://mail.gnu.org/archive/html/autoconf/2000-12/msg00049.html
dnl   and surrounding thread for discussion.  However that doesn't
dnl   appear to work in fact, and the function in getopt_long.c has
dnl   different magic to avoid getting horribly tangled up.
dnl
dnl   Note that the getopt checks must appear _after_ the checks for
dnl   kpathsea.  This is because the kpathsea library provides an
dnl   implementation for getopt so that, if kpathsea is included, we
dnl   must make sure that we pick up that one.  If this test is
dnl   placed before kpathsea, we can end up defining getopt twice.
AC_CHECK_FUNCS(getopt)
AC_CHECK_DECLS([getopt,getsubopt,getopt_long])
AC_REPLACE_FUNCS(getopt_long getsubopt)


dnl   What type of system are we on?
dnl   The alpha cxx compiler currently (v6.0) needs to be invoked with 
dnl   __USE_STD_IOSTREAM defined, so that it uses a standards-conforming 
dnl   streams library.  See Digital C++ v6.0 manual, section 7.1.2.
dnl   This is defined by default when the compiler is invoked with
dnl   `-std strict_ansi', but that's a little harder to arrange in here.
dnl
dnl   I don't quite know how, but AC_CANONICAL_HOST appears to be
dnl   invoked implicitly somewhere above this: if I include it here I
dnl   get a warning from autoconf that it has been invoked multiple
dnl   times.  Just check, though...
if test -z "$host"; then
  echo "Ooops, we _do_ need to include ac_canonical_host again!"
  exit 1
fi
dnl AC_CANONICAL_HOST
case "$host" in
  alpha*) test "$CXX" = "cxx" && AC_DEFINE(__USE_STD_IOSTREAM) ;;
esac

dnl   use C++ to compile the remaining test programs
AC_LANG([C++])

dnl   Do we have (standard) <cname> for C libraries, or just <name.h>?
AC_MSG_CHECKING(whether compiler has <cname> instead of just <name.h>)
AC_TRY_COMPILE([#include <cstdio>],,
  AC_DEFINE(HAVE_CSTD_INCLUDE)
  AC_MSG_RESULT(yes),
  AC_MSG_RESULT(no))

dnl   Are the standard functions in the std namespace?
dnl   We only need to check <ostream> here, but <iostream> is on more
dnl   machines (specifically, and irritatingly, <ostream> appears to
dnl   be missing from egcs-2.91.66)
AC_MSG_CHECKING(whether standard functions are in namespace std)
AC_TRY_COMPILE([#include <iostream>], [cout << "Hello\n";],
  AC_MSG_RESULT(no),
  AC_TRY_COMPILE([#include <iostream>], [using namespace std;cout<<"Hello\n";],
    AC_DEFINE(HAVE_STD_NAMESPACE)
    AC_MSG_RESULT(yes),
    AC_MSG_RESULT(ARGHH -- error compiling)))


dnl   Extra .lo objects, to be conditionally compiled
AC_SUBST(EXTRALO)
EXTRALO=

dnl   Specify support for Kpathsea library
dnl
dnl   The texmfcnf and fake-progname options
dnl   are suppressed if the support for kpathsea is turned off.  This is
dnl   because they largely depend on kpathsea searching being present.
dnl
dnl   Note, however, that font-generation is NOT, now, disabled if kpathsea
dnl   is.  This can create puzzling errors: fonts might be generated even when
dnl   they're not really missing (because the non-kpathsea search mechanism
dnl   can't find them), but the program still can't find them afterwards 
dnl   (because the font-generation method has put them where only kpathsea
dnl   expects to find them).  From experience with user feedback,
dnl   explaining this problem and running the test script in
dnl   test/test-font-gen.sh (`make test'), which explains what
dnl   DVI2BITMAP_PK_PATH should be set to, is less confusing and
dnl   inconvenient than having to rebuild dvi2bitmap with
dnl   font-generation forced on.
dnl
needfontfinding=1
AC_ARG_WITH(kpathsea,
[  --with-kpathsea[[=path]]  Use kpathsea, giving optional path to library
  --without-kpathsea      Do not use kpathsea library],
  if test "$withval" = "yes"; then
    extrapath=default
  elif test "$withval" = "no"; then
    extrapath=
  else
    extrapath=$withval
  fi,
  extrapath=default)
if test -n "$extrapath"; then
  if test "$extrapath" != default; then
    CPPFLAGS="$CPPFLAGS -I$extrapath/include"
    LIBS="$LIBS -L$extrapath/lib"
  fi
  AC_CHECK_HEADER(kpathsea/pathsearch.h,
                  [AC_CHECK_LIB(kpathsea, kpse_find_file,
                                [AC_DEFINE(ENABLE_KPATHSEA)]
                                EXTRALO="$EXTRALO kpathsea.lo"
                                LIBS="$LIBS -lkpathsea"
                                needfontfinding=0)])
fi

dnl   Specify an alternative, or additional, font-finding script.
dnl   This should be expressed as a `font-string template', as
dnl   described in config.h
AC_ARG_WITH(fontfinder,
[  --with-fontfinder=script  Specify a script to find fonts],
  if test "$withval" != yes -a "$withval" != no; then
    [AC_DEFINE_UNQUOTED(FONT_SEARCH_SCRIPT, "$withval")]
    needfontfinding=0
  fi)

dnl   Test whether we have the kpse* commands, by searching through the path
dnl   for kpsewhich
AC_PATH_PROG(KPSEWHICH, kpsewhich)

dnl   If we're not using the kpselib, and we haven't specified a
dnl   fontfinder script, then use kpsewhich
if test $needfontfinding != 0 -a -n "$ac_cv_path_KPSEWHICH"; then
  AC_DEFINE_UNQUOTED(FONT_SEARCH_SCRIPT, "$ac_cv_path_KPSEWHICH pk %f.%dpk")
  needfontfinding=0
fi


dnl   --disable-texmfcnf
if test -n "$ac_cv_path_KPSEWHICH"; then
AC_ARG_ENABLE(texmfcnf,
[  --disable-texmfcnf      Disable setting of TEXMFCNF in environment],
  test "$enableval" = "yes" && T=`kpsewhich cnf texmf.cnf`
  test -n "$T" && AC_DEFINE_UNQUOTED(DEFAULT_TEXMFCNF, "`dirname $T`"),
  T=`kpsewhich cnf texmf.cnf`
  test -n "$T" && AC_DEFINE_UNQUOTED(DEFAULT_TEXMFCNF, "`dirname $T`"))
fi

dnl   Do we enable automatic font-generation?
dnl   If kpathsea is enabled, then the default is yes, otherwise no.
dnl
dnl   Well, not really.  I can't make my mind up about this, but I think
dnl   it's best, in general, if font-generation is always on by default,
dnl   as long as there's enough warning of the problems about this in
dnl   the documentation.
dnl
dnl   This option allows the user to specify a template for the
dnl   font-generation command, in the form of a `font-string
dnl   template', as described in config.h.  If no such template is
dnl   given, then we search for mktexpk and MakeTeXPK in turn, and
dnl   supply the well-known template for whichever one of those we
dnl   find.
dnl
AC_ARG_ENABLE(fontgen,
[  --enable-fontgen[[=template]]
                          Enable automatic font gen., opt. giving template],
  if test "$enableval" = "yes"; then
    getFontGenTemplate=true
  elif test "$enableval" = "no"; then
    getFontGenTemplate=false
  else
    AC_DEFINE_UNQUOTED(FONT_GEN_TEMPLATE, "${enableval}")
    getFontGenTemplate=false
  fi,
  getFontGenTemplate=true)


dnl   --with-mktexpk.  See also --enable-fontgen.
AC_PATH_PROG(MKTEXPK_PATH, mktexpk)
AC_ARG_WITH(mktexpk,
[  --with-mktexpk[[=prog]]   Enable mktexpk (default), giving optional program],
  if test "$withval" = "no"
  then unset MKTEXPK_PATH
dnl  elif test "$withval" != "yes" && -n "$KPATHSEAOBJ"
  elif test "$withval" != "yes"
  then MKTEXPK_PATH=$withval
  fi)

if $getFontGenTemplate && test -n "$MKTEXPK_PATH"; then
  AC_DEFINE_UNQUOTED(FONT_GEN_TEMPLATE, "${MKTEXPK_PATH}  --dpi %d --bdpi %b --mag %m --mfmode %M %f")
  getFontGenTemplate=false  # skip test below
fi

dnl   Use MakeTeXPK.  Don't use this if mktexpk was found
AC_PATH_PROG(MAKETEXPK_PATH, MakeTeXPK)
AC_ARG_WITH(maketexpk,
[  --with-maketexpk[[=prog]]   Enable MakeTeXPK, giving optional alternative],
  if test "$withval" = "no"; then
    unset MAKETEXPK_PATH
  elif test "$withval" != "yes"; then
    MAKETEXPK_PATH=$withval
  fi)

if $getFontGenTemplate && test -n "$MAKETEXPK_PATH"; then
  AC_DEFINE_UNQUOTED(FONT_GEN_TEMPLATE, "${MAKETEXPK_PATH} %f %d %b %m %M")
fi


dnl   Given that we are indeed generating fonts, if we want to use a
dnl   non-default Metafont mode, we can specify it here.
AC_ARG_WITH(fontgen-mode,
[  --with-fontgen-mode=mode,res
                          Specify mode,res for automatic font gen],
  if test "$withval" != "yes" -q "$withval" != "no"; then
    [eval `echo $withval | awk -F, '{printf "GENMODE=%s GENRES=%s",$1,$2}'`]
    AC_DEFINE_UNQUOTED(DEFAULT_MFMODE,"$GENMODE")
    test -n "$GENRES" && AC_DEFINE_UNQUOTED(DEFAULT_RESOLUTION,$GENRES)
  fi)


dnl   Do we want to use the fake_progname feature in kpathsea.cc?
dnl   (following makes no sense if we don't have kpsewhich, and so
dnl   presumably aren't using the texmf.cnf, but this is a
dnl   use-at-your-own-risk option anyway, so presumably the user knows
dnl   what they're doing)
AC_PATH_PROG(XDVI_PATH, xdvi)
AC_ARG_ENABLE(fake-progname,
[  --enable-fake-progname[[=prog]]
                          Have dvi2bitmap lie about its name (see full docs)],
  if test "$enableval" = "yes"
  then AC_DEFINE_UNQUOTED(FAKE_PROGNAME, "${XDVI_PATH}")
  elif test "$enableval" != "no"
  then AC_DEFINE_UNQUOTED(FAKE_PROGNAME, "${enableval}")
  fi)

dnl   Specify support for PNG output.  Test library for function
dnl   png_libpng_ver.  At present, PNGBitmap.cc is
dnl   written to use a palette rather than a greyscale (but see the
dnl   comments about GREYSCALE_BITMAP in that module).  If we were to
dnl   revert to using the colour model PNG_COLOR_TYPE_GRAY, then we
dnl   need the function png_set_write_user_transform_fn (which means later
dnl   than version 0.96).
dnl
dnl   Test this by default (ie, unless the option --disable-png is given).
AC_ARG_WITH(png,
[  --with-png[[=path]]       Generate png files, giving optional path to library
  --without-png           Do not use png library],
  if test "$withval" = "yes"; then
    extrapath=default
  elif test "$withval" = "no"; then
    extrapath=
  else
    extrapath=$withval
  fi,
  extrapath=default)
if test -n "$extrapath"; then
  if test "$extrapath" != default; then
    CPPFLAGS="$CPPFLAGS -I$extrapath/include"
    LIBS="$LIBS -L$extrapath/lib"
  fi
  AC_CHECK_HEADER(png.h,
                  [AC_CHECK_LIB(png, png_libpng_ver,
                                [AC_DEFINE(ENABLE_PNG)]
                                EXTRALO="$EXTRALO PNGBitmap.lo"
                                LIBS="$LIBS -lpng -lz",
                                echo "png library not found or too old -- not including PNG support",
                                -lz)])
fi

dnl   Specify support for GIF output
AC_ARG_ENABLE(gif, 
[  --enable-gif            Enable output of GIF files],
  [AC_DEFINE(ENABLE_GIF)]
  EXTRALO="$EXTRALO GIFBitmap.lo"
  )


dnl   Select path separator characters.  Specify as a pair of
dnl   characters, first filesystem-path, then search path.  For
dnl   example, unix would have --with-path-seps=/:
AC_DEFINE(FSPATH_SEP,'/')
AC_DEFINE(SRCHPATH_SEP,':')
AC_ARG_WITH(path-seps,
[  --with-path-seps=cc     Specify path separators (default --with-path-seps=/:)],
  if test "$withval" != "yes" -q "$withval" != "no"; then
    # xx below allows us to deal with the case where $withval has other
    # than two characters.  Note no \? in basic REs.
    # Quoting below is subtle -- note that eval re-parses its argument.
    [eval `echo "$withval"xx | sed "s/\(.\)\(.\).*/SLASH='\\1' COLON='\\2'/"`]
    AC_DEFINE_UNQUOTED(FSPATH_SEP,'$SLASH')
    test -n "$COLON" && AC_DEFINE_UNQUOTED(SRCHPATH_SEP,'$COLON')
  fi)

dnl   DOXYGEN_OUTPUT_DIRECTORY is the directory into which Doxygen
dnl   output will be placed
AC_SUBST(DOXYGEN_OUTPUT_DIRECTORY)
DOXYGEN_OUTPUT_DIRECTORY="doc_libdvi2bitmap"

AC_CONFIG_FILES([Makefile test/Makefile Doxyfile version.h])
AC_OUTPUT

