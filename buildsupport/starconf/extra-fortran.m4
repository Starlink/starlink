# This file is part of Autoconf.                       -*- Autoconf -*-
#
#
# Hacked at by Norman Gray, to add fpp support, adapting the autoconf
# patches submitted to autoconf-patches@gnu.org by Martin Wilck (see
# <http://sources.redhat.com/ml/autoconf-patches/2000-07/msg00287.html>
# and the following thread).
#
# The modifications are as follows:
#
# - Change define to m4_define, to match newer (post 2.50) versions of autoconf
# - Change .F to .fpp (on case-insensitive filesystems such as HFS+,
#   file.f and file.F are the same file)
# - Slight changes to tests, following comments by Akim Demaille
#   on the mailing list (including Martin's cunning
#   _AC_SHELL_POSITIONAL).
# - renamed $ac_gnu_compiler to $ac_compiler_gnu
# - Added _AC_LANG_ABBREV(Preprocessed Fortran 77), and a few other
#   functions which match the support apparatus seems to want
# - Added AC_LANG_PREPROC and AC_LANG_COMPILER
# - Added AC_REQUIRE of _AC_PROG_F77_CPP to _AC_PROG_FPP
# - In test of how to run the preprocessor alone, check that our tests
#   don't produce a a.out file -- if they do, that counts as a
#   failure, even with a zero exit status
# - Removed _AC_ECHO calls -- were always internal, now disappeared
# - Changed old >&AC_FD_LOG to >&AS_MESSAGE_LOG_FD
#
# Note that if these macros are included in the source directory, for
# processing by aclocal, then the version of aclocal must be 1.7 or
# better (I think -- 1.6.3 doesn't work, 1.7.5 does), or else the
# spaces in macro names such as [AC_LANG(Preprocessed Fortran 77)]
# confuses aclocal.
#
#
#
# Fortran languages support.
# Copyright 2001
# Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
#
# As a special exception, the Free Software Foundation gives unlimited
# permission to copy, distribute and modify the configure scripts that
# are the output of Autoconf.  You need not follow the terms of the GNU
# General Public License when using or distributing such scripts, even
# though portions of the text of Autoconf appear in them.  The GNU
# General Public License (GPL) does govern all other use of the material
# that constitutes the Autoconf program.
#
# Certain portions of the Autoconf source text are designed to be copied
# (in certain cases, depending on the input) into the output of
# Autoconf.  We call these the "data" portions.  The rest of the Autoconf
# source text consists of comments plus executable code that decides which
# of the data portions to output in any given case.  We call these
# comments and executable code the "non-data" portions.  Autoconf never
# copies any of the non-data portions into its output.
#
# This special exception to the GPL applies to versions of Autoconf
# released by the Free Software Foundation.  When you make and
# distribute a modified version of Autoconf, you may extend this special
# exception to the GPL to apply to your modified version as well, *unless*
# your modified version has the potential to copy into its output some
# of the text that was the non-data portion of the version that you started
# with.  (In other words, unless your change moves or copies text from
# the non-data portions to the data portions.)  If your modification has
# such potential, you must delete any notice of this special exception
# to the GPL from your modified version.
#
# Written by David MacKenzie, with help from
# Franc,ois Pinard, Karl Berry, Richard Pixley, Ian Lance Taylor,
# Roland McGrath, Noah Friedman, david d zuhn, and many others.


# _AC_LIST_MEMBER_IF(ELEMENT, LIST, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
# ---------------------------------------------------------------------------
#
# Processing the elements of a list is tedious in shell programming,
# as lists tend to be implemented as space delimited strings.
#
# This macro searches LIST for ELEMENT, and executes ACTION-IF-FOUND
# if ELEMENT is a member of LIST, otherwise it executes
# ACTION-IF-NOT-FOUND.
AC_DEFUN([_AC_LIST_MEMBER_IF],
[dnl Do some sanity checking of the arguments.
m4_if([$1], , [AC_FATAL([$0: missing argument 1])])dnl
m4_if([$2], , [AC_FATAL([$0: missing argument 2])])dnl
  ac_exists=false
  for ac_i in $2; do
    if test x"$1" = x"$ac_i"; then
      ac_exists=true
      break
    fi
  done

  AS_IF([test x"$ac_exists" = xtrue], [$3], [$4])[]dnl
])# _AC_LIST_MEMBER_IF


# _AC_LINKER_OPTION(LINKER-OPTIONS, SHELL-VARIABLE)
# -------------------------------------------------
#
# Specifying options to the compiler (whether it be the C, C++ or
# Fortran 77 compiler) that are meant for the linker is compiler
# dependent.  This macro lets you give options to the compiler that
# are meant for the linker in a portable, compiler-independent way.
#
# This macro take two arguments, a list of linker options that the
# compiler should pass to the linker (LINKER-OPTIONS) and the name of
# a shell variable (SHELL-VARIABLE).  The list of linker options are
# appended to the shell variable in a compiler-dependent way.
#
# For example, if the selected language is C, then this:
#
#   _AC_LINKER_OPTION([-R /usr/local/lib/foo], foo_LDFLAGS)
#
# will expand into this if the selected C compiler is gcc:
#
#   foo_LDFLAGS="-Xlinker -R -Xlinker /usr/local/lib/foo"
#
# otherwise, it will expand into this:
#
#   foo_LDFLAGS"-R /usr/local/lib/foo"
#
# You are encouraged to add support for compilers that this macro
# doesn't currently support.
# FIXME: Get rid of this macro.
AC_DEFUN([_AC_LINKER_OPTION],
[if test "$ac_compiler_gnu" = yes; then
  for ac_link_opt in $1; do
    $2="[$]$2 -Xlinker $ac_link_opt"
  done
else
  $2="[$]$2 $1"
fi[]dnl
])# _AC_LINKER_OPTION



## ----------------------- ##
## 1. Language selection.  ##
## ----------------------- ##


# ----------------------------- #
# 1d. The Fortran 77 language.  #
# ----------------------------- #


# AC_LANG(Fortran 77)
# -------------------
m4_define([AC_LANG(Fortran 77)],
[ac_ext=f
ac_compile='$F77 -c $FFLAGS conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
ac_link='$F77 -o conftest$ac_exeext $FFLAGS $LDFLAGS conftest.$ac_ext $LIBS >&AS_MESSAGE_LOG_FD'
ac_compiler_gnu=$ac_cv_f77_compiler_gnu
])


# AC_LANG_FORTRAN77
# -----------------
AU_DEFUN([AC_LANG_FORTRAN77], [AC_LANG(Fortran 77)])


# _AC_LANG_ABBREV(Fortran 77)
# ---------------------------
m4_define([_AC_LANG_ABBREV(Fortran 77)], [f77])



# ------------------------------------------ #
# 1e. The Preprocessed Fortran 77 language.  #
# ------------------------------------------ #
#
# We need a separate `preprocessed' language, because not all Fortran 
# compilers have a preprocessor built in.  Therefore we may need to
# resort to an `indirect' compilation, .fpp->.f->.o, including the
# generation of a suitable extra build rule.

# AC_LANG(Preprocessed Fortran 77)
# --------------------------------
m4_define([AC_LANG(Preprocessed Fortran 77)],
[ac_ext=fpp
# We need to use variables because compilation depends on whether 
# $F77 supports direct compilation of source with cpp directives
ac_compile=$ac_ppf77_compile
ac_link=$ac_ppf77_link
ac_compiler_gnu=$ac_cv_prog_g77
])


# AC_LANG_FORTRAN77_FPP
# ---------------------
AU_DEFUN([AC_LANG_FORTRAN77_FPP], [AC_LANG(Preprocessed Fortran 77)])


# _AC_LANG_ABBREV(Preprocessed Fortran 77)
# ----------------------------------------
m4_define([_AC_LANG_ABBREV(Preprocessed Fortran 77)], [ppf77])




## ---------------------- ##
## 2.Producing programs.  ##
## ---------------------- ##


# ------------------------ #
# 2d. Fortran 77 sources.  #
# ------------------------ #

# AC_LANG_SOURCE(Fortran 77)(BODY)
# --------------------------------
# FIXME: Apparently, according to former AC_TRY_COMPILER, the CPP
# directives must not be included.  But AC_TRY_RUN_NATIVE was not
# avoiding them, so?
m4_define([AC_LANG_SOURCE(Fortran 77)],
[$1])


# AC_LANG_PROGRAM(Fortran 77)([PROLOGUE], [BODY])
# -----------------------------------------------
# Yes, we discard the PROLOGUE.
m4_define([AC_LANG_PROGRAM(Fortran 77)],
[m4_ifval([$1],
       [m4_warn([syntax], [$0: ignoring PROLOGUE: $1])])dnl
      program main
$2
      end])


# AC_LANG_CALL(Fortran 77)(PROLOGUE, FUNCTION)
# --------------------------------------------
# FIXME: This is a guess, help!
# But it's a good guess -- what's the problem?
m4_define([AC_LANG_CALL(Fortran 77)],
[AC_LANG_PROGRAM([$1],
[      call $2])])


# ------------------------------------- #
# 2e. Preprocessed Fortran 77 sources.  #
# ------------------------------------- #


# AC_LANG_SOURCE(Preprocessed Fortran 77)(BODY)
# ---------------------------------------------
# We don't use an include statement here, because some Fortran
# preprocessors (e.g. Lahey) don't properly handle it
# We don't use a line directive either, since fpp doesn't understand it.
m4_define([AC_LANG_SOURCE(Preprocessed Fortran 77)],
[$1])


# AC_LANG_PROGRAM(Preprocessed Fortran 77)([PROLOGUE], [BODY])
# ------------------------------------------------------------
# This time, we do not discard the PROLOGUE.
m4_define([AC_LANG_PROGRAM(Preprocessed Fortran 77)],
[$1
      program main
$2
      end])


# AC_LANG_CALL(Preprocessed Fortran 77)(PROLOGUE, FUNCTION)
# --------------------------------------------
# FIXME: This is a guess, help!
# But it's a good guess -- what's the problem?
m4_copy([AC_LANG_CALL(Fortran 77)], 
        [AC_LANG_CALL(Preprocessed Fortran 77)])



## -------------------------------------------- ##
## 3. Looking for Compilers and Preprocessors.  ##
## -------------------------------------------- ##


# ----------------------------- #
# 3d. The Fortran 77 compiler.  #
# ----------------------------- #


# AC_LANG_PREPROC(Fortran 77)
# ---------------------------
# Find the Fortran 77 preprocessor.  Must be AC_DEFUN'd to be AC_REQUIRE'able.
AC_DEFUN([AC_LANG_PREPROC(Fortran 77)],
[m4_warn([syntax],
         [$0: No preprocessor defined for ]_AC_LANG)])


# AC_LANG_COMPILER(Fortran 77)
# ----------------------------
# Find the Fortran 77 compiler.  Must be AC_DEFUN'd to be
# AC_REQUIRE'able.
AC_DEFUN([AC_LANG_COMPILER(Fortran 77)],
[AC_REQUIRE([AC_PROG_F77])])


# ac_cv_prog_g77
# --------------
# We used to name the cache variable this way.
AU_DEFUN([ac_cv_prog_g77],
[ac_cv_f77_compiler_gnu])


# AC_PROG_F77([COMPILERS...])
# ---------------------------
# COMPILERS is a space separated list of Fortran 77 compilers to search
# for.  Fortran 95 isn't strictly backwards-compatible with Fortran 77,
# but `f95' is worth trying.
#
# Compilers are ordered by
#  1. F77, F90, F95
#  2. Good/tested native compilers, bad/untested native compilers
#  3. Wrappers around f2c go last.
#
# `fort77' is a wrapper around `f2c'.
# It is believed that under HP-UX `fort77' is the name of the native
# compiler.  On some Cray systems, fort77 is a native compiler.
# frt is the Fujitsu F77 compiler.
# pgf77 and pgf90 are the Portland Group F77 and F90 compilers.
# xlf/xlf90/xlf95 are IBM (AIX) F77/F90/F95 compilers.
# lf95 is the Lahey-Fujitsu compiler.
# fl32 is the Microsoft Fortran "PowerStation" compiler.
# af77 is the Apogee F77 compiler for Intergraph hardware running CLIX.
# epcf90 is the "Edinburgh Portable Compiler" F90.
# fort is the Compaq Fortran 90 (now 95) compiler for Tru64 and Linux/Alpha.
AC_DEFUN([AC_PROG_F77],
[AC_LANG_PUSH(Fortran 77)dnl
AC_ARG_VAR([F77],    [Fortran 77 compiler command])dnl
AC_ARG_VAR([FFLAGS], [Fortran 77 compiler flags])dnl
_AC_ARG_VAR_LDFLAGS()dnl
AC_CHECK_TOOLS(F77,
      [m4_default([$1],
                  [g77 f77 xlf frt pgf77 fl32 af77 fort77 f90 xlf90 pgf90 epcf90 f95 fort xlf95 lf95 g95])])

# Provide some information about the compiler.
echo "$as_me:__oline__:" \
     "checking for _AC_LANG compiler version" >&AS_MESSAGE_LOG_FD
ac_compiler=`set X $ac_compile; echo $[2]`
_AC_EVAL([$ac_compiler --version </dev/null >&AS_MESSAGE_LOG_FD])
_AC_EVAL([$ac_compiler -v </dev/null >&AS_MESSAGE_LOG_FD])
_AC_EVAL([$ac_compiler -V </dev/null >&AS_MESSAGE_LOG_FD])

m4_expand_once([_AC_COMPILER_EXEEXT])[]dnl
m4_expand_once([_AC_COMPILER_OBJEXT])[]dnl
# If we don't use `.F' as extension, the preprocessor is not run on the
# input file.
ac_save_ext=$ac_ext
ac_ext=F
_AC_LANG_COMPILER_GNU
ac_ext=$ac_save_ext
G77=`test $ac_compiler_gnu = yes && echo yes`
_AC_PROG_F77_G
AC_LANG_POP(Fortran 77)dnl
])# AC_PROG_F77


# _AC_PROG_F77_G
# --------------
# Check whether -g works, even if FFLAGS is set, in case the package
# plays around with FFLAGS (such as to build both debugging and normal
# versions of a library), tasteless as that idea is.
m4_define([_AC_PROG_F77_G],
[ac_test_FFLAGS=${FFLAGS+set}
ac_save_FFLAGS=$FFLAGS
FFLAGS=
AC_CACHE_CHECK(whether $F77 accepts -g, ac_cv_prog_f77_g,
[FFLAGS=-g
_AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
[ac_cv_prog_f77_g=yes],
[ac_cv_prog_f77_g=no])
])
if test "$ac_test_FFLAGS" = set; then
  FFLAGS=$ac_save_FFLAGS
elif test $ac_cv_prog_f77_g = yes; then
  if test "$G77" = yes; then
    FFLAGS="-g -O2"
  else
    FFLAGS="-g"
  fi
else
  if test "$G77" = yes; then
    FFLAGS="-O2"
  else
    FFLAGS=
  fi
fi[]dnl
])# _AC_PROG_F77_G


# AC_PROG_F77_C_O
# ---------------
# Test if the Fortran 77 compiler accepts the options `-c' and `-o'
# simultaneously, and define `F77_NO_MINUS_C_MINUS_O' if it does not.
#
# The usefulness of this macro is questionable, as I can't really see
# why anyone would use it.  The only reason I include it is for
# completeness, since a similar test exists for the C compiler.
AC_DEFUN([AC_PROG_F77_C_O],
[AC_REQUIRE([AC_PROG_F77])dnl
AC_CACHE_CHECK([whether $F77 understand -c and -o together],
               [ac_cv_prog_f77_c_o],
[AC_LANG_CONFTEST([AC_LANG_PROGRAM([])])
# We test twice because some compilers refuse to overwrite an existing
# `.o' file with `-o', although they will create one.
ac_try='$F77 $FFLAGS -c conftest.$ac_ext -o conftest.$ac_objext >&AS_MESSAGE_LOG_FD'
if AC_TRY_EVAL(ac_try) &&
     test -f conftest.$ac_objext &&
     AC_TRY_EVAL(ac_try); then
  ac_cv_prog_f77_c_o=yes
else
  ac_cv_prog_f77_c_o=no
fi
rm -f conftest*])
if test $ac_cv_prog_f77_c_o = no; then
  AC_DEFINE(F77_NO_MINUS_C_MINUS_O, 1,
            [Define to 1 if your Fortran 77 compiler doesn't accept
             -c and -o together.])
fi
])# AC_PROG_F77_C_O



# ----------------------------------------- #
# 3e. The Preprocessed Fortran 77 compiler. #
# ----------------------------------------- #


# AC_LANG_PREPROC(Preprocessed Fortran 77)
# ----------------------------------------
# Find the Fortran 77 preprocessor.  Must be AC_DEFUN'd to be AC_REQUIRE'able.
AC_DEFUN([AC_LANG_PREPROC(Preprocessed Fortran 77)],
[AC_REQUIRE([AC_PROG_FPP])])


# AC_LANG_COMPILER(Preprocessed Fortran 77)
# -----------------------------------------
# Find the Fortran 77 compiler.  Must be AC_DEFUN'd to be
# AC_REQUIRE'able.
AC_DEFUN([AC_LANG_COMPILER(Preprocessed Fortran 77)],
[AC_REQUIRE([AC_PROG_F77])])


# --- FPP macros

# ------------------------------------------#
# Some test programs for different features #
# ------------------------------------------#

# _AC_LANG_PROGRAM_FPP_SIMPLE
# ---------------------------
# The minimum test program - any compiler supporting
# preprocessing should handle this
AC_DEFUN([_AC_LANG_PROGRAM_FPP_SIMPLE],
[AC_LANG_PROGRAM([
#define OK
], [#ifndef OK
      syntax error
#endif
])]) #_AC_LANG_PROGRAM_FPP_SIMPLE


# _AC_LANG_PROGRAM_FPP_ONLY
# ---------------------------
# Test program for pure preprocessing
AC_DEFUN([_AC_LANG_PROGRAM_FPP_ONLY],
[AC_LANG_PROGRAM([
#define OK
], [#ifdef OK
      REAL A
#else
      syntax error
#endif
])]) #_AC_LANG_PROGRAM_FPP_ONLY


# _AC_LANG_PROGRAM_FPP_D
# ---------------------------
# Like _AC_LANG_PROGRAM_FPP_SIMPLE, but OK is passed via -D switch
AC_DEFUN([_AC_LANG_PROGRAM_FPP_D],
[AC_LANG_PROGRAM([],[
#ifndef OK
      syntax error
#endif
])]) #_AC_LANG_PROGRAM_FPP_D


# _AC_LANG_PROGRAM_FPP_I
# ---------------------------
# Test for #include statement
# If unsupported, this should give a type error
AC_DEFUN([_AC_LANG_PROGRAM_FPP_I],
[AC_LANG_PROGRAM([],[
      IMPLICIT CHARACTER (c)
c conftest.inc contains the Fortran statement "REAL cc"
#include "conftest.inc"
      cc=1.
])]) #_AC_LANG_PROGRAM_FPP_I


# _AC_LANG_PROGRAM_FPP_SUBS
# ---------------------------
# Test whether cpp symbols are expanded in Fortran code lines
# If not, this should give a type error
AC_DEFUN([_AC_LANG_PROGRAM_FPP_SUBS],
[AC_LANG_PROGRAM([
#define NM xxxx
], [      IMPLICIT CHARACTER (n)
      REAL xxxx
      NM=1.
])]) #_AC_LANG_PROGRAM_FPP_SUBS


# _AC_LANG_PROGRAM_FPP_WRAP
# ---------------------------
# Test whether preprocessor breaks lines that become too long due
# to macro substitution. 
# If not, this gives an "unterminated character constant" error
AC_DEFUN([_AC_LANG_PROGRAM_FPP_WRAP],
[AC_LANG_PROGRAM([
#define LONG '901234567890123456789012345678901234567890123456789012345678901234567890'
],[      CHARACTER*80 A
      A=LONG
])]) #_AC_LANG_PROGRAM_FPP_WRAP


# _AC_LANG_PROGRAM_FPP_CSTYLE
# ---------------------------
# Test program for C style comments
AC_DEFUN([_AC_LANG_PROGRAM_FPP_CSTYLE],
[AC_LANG_PROGRAM([],[
      A=1. /* C-style comment */
])]) #_AC_LANG_PROGRAM_FPP_CSTYLE


# ----------------#
# Internal macros #
# ----------------#


# _AC_PROG_FPP_FEATURES ([feature list])
# --------------------------------------
# Parse the feature list from configure.in
AC_DEFUN([_AC_PROG_FPP_FEATURES],
[# defaults for needed features
ac_fpp_need_d=yes
ac_fpp_need_i=yes
ac_fpp_need_subs=yes
ac_fpp_need_wrap=no
ac_fpp_need_cstyle=no
ac_fpp_need_CSTYLE=no
for _t in $1 nil
do
    case $_t in
        d*)    ac_fpp_need_d=yes    ;;
        nod*)  ac_fpp_need_d=no     ;;
        i*)    ac_fpp_need_i=yes    ;;   
        noi*)  ac_fpp_need_i=no     ;;
        s*)    ac_fpp_need_subs=yes    ;;   
        nos*)  ac_fpp_need_subs=no     ;;
        w*)    ac_fpp_need_wrap=yes   ;;   
        now*)  ac_fpp_need_wrap=no    ;;   
        c*)    ac_fpp_need_cstyle=yes ;;   
        noc*)  ac_fpp_need_cstyle=no  ;;   
        C*)    ac_fpp_need_CSTYLE=yes ;;   
        noC*)  ac_fpp_need_CSTYLE=no  ;;   
        nil)   ;;
    esac
done
# Wrapping requires substitution
test $ac_fpp_need_wrap = yes && ac_fpp_need_subs=yes
# Both CSTYLE and cstyle cannot be requested
# CSTYLE has precedence, since if it is not fulfilled,
# compile errors may arise
test $ac_fpp_need_CSTYLE = yes && ac_fpp_need_cstyle=no
]) # _AC_PROG_FPP_FEATURES


# _AC_TEST_FPP ([command])
# ------------------------
# A helper macro to test correct fpp behaviour
# It sets ac_cv_prog_fpp and ac_fpp_out
AC_DEFUN([_AC_TEST_FPP],
[cat >conftest.fpp <<EOF
_AC_LANG_PROGRAM_FPP_ONLY
EOF
ac_fpp_command=$1
if eval '$ac_fpp_command conftest.fpp > conftest.log 2>/dev/null'; then
  test -f conftest.f &&
      cat conftest.f | grep '^      REAL A' >/dev/null 2>&1 &&
      cat conftest.f | grep 'syntax error' >/dev/null 2>&1  ||
    ac_cv_prog_fpp=$ac_fpp_command; ac_fpp_out=
  test -f conftest.log &&
      cat conftest.log | grep '^      REAL A' >/dev/null 2>&1 &&
      cat conftest.log | grep 'syntax error' >/dev/null 2>&1  ||
    ac_cv_prog_fpp=$ac_fpp_command; ac_fpp_out=' > conftest.f'
fi
rm -f conftest*
]) # _AC_TEST_FPP


# _AC_PROG_FPP
# ------------
# Try to figure out how to preprocess .fpp files for use with the selected
# Fortran 77 compiler
#
# Must be run after _AC_PROG_F77_CPP
AC_DEFUN([_AC_PROG_FPP],
[AC_REQUIRE([_AC_PROG_F77_CPP])dnl
AC_CACHE_CHECK([how to preprocess Fortran 77 files], ac_cv_prog_fpp,
[ac_cv_prog_fpp=
AC_LANG_PUSH(Preprocessed Fortran 77)

# Let the user specify FPP
if test -n "$FPP"; then
  _AC_TEST_FPP([$FPP])
  if test -z "$ac_cv_prog_fpp"; then
    AC_MSG_WARN([user-specified \$FPP ($FPP) does not work])
    FPP=
  fi
fi # test -n "$FPP"

if test -z "$ac_cv_prog_fpp" && test $ac_fpp_ok = yes; then
# We know that the Fortran compiler can directly preprocess and compile.
# All that remains is to find out how to run only the preprocessor.
# We check that the compiler does not compile as well as
# preprocessing, by looking for the file a.out (is this portable?  The
# single-unix spec says that cc produces an a.out if no -o is given).
# If we don't do this, and the compiler doesn't recognise, and
# therefore ignores, one of the options (for example g77 ignores -F
# and returns without error), then the test appears to succeed.
#
# We only know the following methods of invocation: -F and -E
  for ac_j in "$F77 -F" "$F77 -E"; do
    rm -f a.out
    _AC_TEST_FPP([$ac_j])
    if test -e a.out; then
        rm -f a.out
        ac_cv_prog_fpp=  # discard any value there
    else
        test -n "$ac_cv_prog_fpp" && break;
    fi
  done
fi

if test -z "$ac_cv_prog_fpp"; then
# Either the Fortran compiler can't handle cpp, or doesn't have all the
# features, or can't be used for pure preprocessing.
# We must find another way for preprocessing.
# We try the "best" preprocessors first. At this point, F77 has already
# proven that it is insufficient, so use it as a last resort only.
# Again, test that no 
  for ac_j in 'fpp' "$CPP" 'g77 -E' '$CC -E' 'cpp' '/lib/cpp' \
              '/usr/ccs/lib/cpp' "$F77 -F" "$F77 -E"; do
    _AC_TEST_FPP([$ac_j])
    test -n "$ac_cv_prog_fpp" && break;
  done
fi # test -z "$ac_cv_prog_fpp"

if test -z "$ac_cv_prog_fpp"; then
# This is only fatal if direct compilation doesn't work either
  if test $ac_cv_prog_f77_cpp = no; then
    AC_MSG_ERROR([cannot find a working Fortran preprocessor])
  else
    AC_MSG_WARN([cannot find a working Fortran preprocessor])
  fi
fi
AC_LANG_POP()dnl
])
AC_CACHE_CHECK([how to redirect $ac_cv_prog_fpp output],
  ac_cv_fpp_out, 
  [ac_cv_fpp_out=$ac_fpp_out])
FPP=$ac_cv_prog_fpp
ac_fpp_out=$ac_cv_fpp_out
]) # _AC_PROG_FPP


# _AC_PROG_FPP_P
# --------------
# Check whether we need "-P" to produce code that F77 compiles
AC_DEFUN([_AC_PROG_FPP_P],
[AC_CACHE_CHECK([how to produce code that $F77 can compile],
ac_cv_prog_fpp_p,
[ac_cv_prog_fpp_p=unknown
AC_LANG_PUSH(Preprocessed Fortran 77)
AC_LANG_ASSERT(Preprocessed Fortran 77)
cat > conftest.fpp << EOF
_AC_LANG_PROGRAM_FPP_ONLY
EOF
AC_LANG_POP()dnl

AC_LANG_PUSH(Fortran 77)
ac_cmd='$FPP $FPPFLAGS conftest.fpp '"$ac_fpp_out"
if AC_TRY_EVAL(ac_cmd) &&
     AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext}; then
   ac_cv_prog_fpp_p=
else
   ac_save_FPPFLAGS=$FPPFLAGS
   FPPFLAGS="$FPPFLAGS -P"
   ac_cmd='$FPP $FPPFLAGS conftest.fpp '"$ac_fpp_out"
   if AC_TRY_EVAL(ac_cmd) &&
       AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext}; then
     ac_cv_prog_fpp_p=-P
   fi
   FPPFLAGS=$ac_save_FPPFLAGS   
fi
rm -f conftest*
AC_LANG_POP()dnl
])
if test "x$ac_cv_prog_fpp_p" = "xunknown"; then
   AC_MSG_ERROR([$FPP cannot produce code that $F77 compiles])
else
   FPPFLAGS="$FPPFLAGS $ac_cv_prog_fpp_p"
fi
]) # _AC_PROG_FPP_P

# _AC_PROG_FPP_CSTYLE
# -------------------
# Check whether FPP lets C-style options through to F77
AC_DEFUN([_AC_PROG_FPP_CSTYLE],
[AC_CACHE_CHECK([how to pass C-style comments to $F77], 
   ac_cv_prog_fpp_cstyle,
[ac_cv_prog_fpp_cstyle=unknown
AC_LANG_PUSH(Preprocessed Fortran 77)
cat > conftest.fpp << EOF
_AC_LANG_PROGRAM_FPP_CSTYLE
EOF
AC_LANG_POP()dnl

AC_LANG_PUSH(Fortran 77)
ac_cmd='$FPP $FPPFLAGS conftest.fpp '"$ac_fpp_out"
if AC_TRY_EVAL(ac_cmd) &&
   cat conftest.f | grep '[[*]]/.*[[*]]/' >/dev/null 2>&1; then
   ac_cv_prog_fpp_cstyle=
else
   ac_save_FPPFLAGS=$FPPFLAGS
   ac_name=`expr "x$FPP" : 'x\(fpp\)'` 
   if test "x$ac_name" = xfpp; then
     ac_flag="-c_com=no"
   else
     ac_flag="-C"
   fi
   FPPFLAGS="$FPPFLAGS $ac_flag"
   ac_cmd='$FPP $FPPFLAGS conftest.fpp '"$ac_fpp_out"
   if AC_TRY_EVAL(ac_cmd) &&
     cat conftest.f | grep '/[[*]].*[[*]]/' >/dev/null 2>&1; then
     ac_cv_prog_fpp_cstyle=$ac_flag
   fi
   FPPFLAGS=$ac_save_FPPFLAGS   
fi
rm -f conftest*
AC_LANG_POP()dnl
])
if test "x$ac_cv_prog_fpp_cstyle" = "xunknown"; then
  AC_MSG_WARN([cannot find a way to make $FPP pass C-style comments])
else
  FPPFLAGS="$FPPFLAGS $ac_cv_prog_fpp_cstyle"
fi
]) # _AC_PROG_FPP_CSTYLE

# _AC_PROG_F77_CPP
# ----------------
# Test whether compilation of Fortan code with preprocessor directives
# succeeds, and check for supported features
# 
# Sets ac_fpp_ok to "no" if a requested feature is unavailable
#
AC_DEFUN([_AC_PROG_F77_CPP],
[ac_fpp_ok=yes
ac_prog_f77_cpp=no
ac_prog_f77_cpp_d=no
ac_prog_f77_cpp_i=no
ac_prog_f77_cpp_subs=no
ac_prog_f77_cpp_wrap=no
ac_prog_f77_cpp_CSTYLE=no

AC_LANG_PUSH(Preprocessed Fortran 77)
AC_MSG_CHECKING([for preprocessor features])
# We must use AC_LINK_IFELSE because Lahey Fortran (and maybe others) have
# broken exit status when compiling
AC_LINK_IFELSE([_AC_LANG_PROGRAM_FPP_SIMPLE], 
   [ac_prog_f77_cpp=yes],
   [ac_fpp_ok=no]) 

if test $ac_prog_f77_cpp = yes; then

    if test $ac_fpp_need_d = yes; then
       ac_prog_f77_cpp_d=no
       ac_save_FPPFLAGS=$FPPFLAGS
       FPPFLAGS="$FPPFLAGS -DOK"
       AC_LINK_IFELSE([_AC_LANG_PROGRAM_FPP_D],
         [ac_prog_f77_cpp_d=yes], 
         [ac_fpp_ok=no])
       FPPFLAGS=$ac_save_FPPFLAGS
    fi

    if test $ac_fpp_need_i = yes; then
       mkdir conftst
       cat > conftst/conftest.inc <<EOF
c This statement overrides the IMPLICIT statement in the program
      REAL cc
EOF
       ac_save_FPPFLAGS=$FPPFLAGS
       FPPFLAGS="$FPPFLAGS -Iconftst"
       AC_LINK_IFELSE([_AC_LANG_PROGRAM_FPP_I],
         [ac_prog_f77_cpp_i=yes],
         [ac_fpp_ok=no])
       rm -rf conftst
       FPPFLAGS=$ac_save_FPPFLAGS
    fi

    if test $ac_fpp_need_subs = yes; then
        AC_LINK_IFELSE([_AC_LANG_PROGRAM_FPP_SUBS],
           [ac_prog_f77_cpp_subs=yes], 
           [ac_fpp_ok=no])
    fi

    if test $ac_fpp_need_wrap = yes; then
        AC_LINK_IFELSE([_AC_LANG_PROGRAM_FPP_WRAP],
           [ac_prog_f77_cpp_wrap=yes], 
           [ac_fpp_ok=no])
    fi

    if test $ac_fpp_need_CSTYLE = yes; then
        AC_LINK_IFELSE([_AC_LANG_PROGRAM_FPP_CSTYLE],
           [ac_prog_f77_cpp_CSTYLE=yes], 
           [ac_fpp_ok=no])
    fi

fi
AC_MSG_RESULT([done.])
AC_LANG_POP()dnl
]) #_AC_PROG_F77_CPP


# _AC_FPP_BUILD_RULE
# ------------------
# Figure out how to build from cpp/Fortran sources
AC_DEFUN([_AC_FPP_BUILD_RULE],
[AC_CACHE_CHECK([how to build from preprocessed Fortran sources],
  ac_cv_fpp_build_rule,
[ac_cv_fpp_build_rule=
if test $ac_cv_prog_f77_cpp_ok = yes; then
   ac_cv_fpp_build_rule=direct
   ac_fpp_status=ok
elif test $ac_cv_prog_fpp_ok = yes; then
   ac_cv_fpp_build_rule=indirect
   ac_fpp_status=ok
elif test $ac_cv_prog_f77_cpp = no && $ac_cv_prog_fpp = no; then
   ac_cv_fpp_build_rule=
   ac_fpp_status=fatal
elif test $ac_cv_prog_f77_cpp = no; then
   ac_cv_fpp_build_rule=indirect
   ac_fpp_status=fail
elif test -z "$ac_cv_prog_fpp"; then
   ac_cv_fpp_build_rule=direct
   ac_fpp_status=fail
elif test $ac_fpp_equals_f77 = yes; then
   ac_cv_fpp_build_rule=direct
   ac_fpp_status=fail
else   
   ac_fpp_status=fail
# Both methods - direct and indirect - fail to meet some requirements
# We use a simple approach to choose the best alternative
   ac_f77_score=0
   ac_fpp_score=0
   for ac_i in d i subs wrap cstyle CSTYLE; do
      ac_tmp="\$ac_fpp_need_$ac_i"
      ac_need=`eval echo $ac_tmp`
      if test $ac_need = yes; then
        ac_tmp="\$ac_cv_prog_f77_cpp_$ac_i"
        ac_v_f77=`eval echo $ac_tmp`
        ac_tmp="\$ac_cv_prog_fpp_$ac_i"
        ac_v_fpp=`eval echo $ac_tmp`
        test "x$ac_v_f77" = xyes && ac_f77_score=`expr $ac_f77_score + 1`
        test "x$ac_v_fpp" = xyes && ac_fpp_score=`expr $ac_fpp_score + 1`
      fi
   done
   if test $ac_fpp_score -gt $ac_f77_score; then
     ac_cv_fpp_build_rule=indirect
   else
     ac_cv_fpp_build_rule=direct
   fi
fi   
])

if test $ac_fpp_status = fatal; then

  AC_MSG_ERROR([cannot find a valid build rule for Fortran/cpp source: consider installing the free Fortran preprocessor fpp from ftp.netlib.org])
	
elif test $ac_fpp_status = fail; then

  AC_MSG_WARN([cannot find a build rule for Fortran/cpp source that fulfills all requirements.  The compilation may fail or run time errors may arise.  Consider installing the free Fortran preprocessor fpp from ftp.netlib.org])

fi

# FPP_SOURCE_EXT is used by automake by generating explicit rules for each
# object file from .fpp source:
# FILE.o:	FILE.@FPP_SOURCE_EXT@
# This is necessary to override make's builtin rules in a portable manner
#   (i.e. without using make extensions)
#
# FPP_MAKE_FLAGS is used to include CPP/FPP related flags into the compiler
# call if we compile directly, and leave them out otherwise.
#
# FPP_OUTPUT is used to redirect FPP output to the .f file in case FPP
# writes to stdout
#
# The make variable \$(ALL_CPPFLAGS) is a hack - I couldn't figure out how to
# add $(AM_CPPFLAGS) to the list without producing autoconf errors
if test $ac_cv_fpp_build_rule = direct; then
    FPP_SOURCE_EXT="fpp"
    FPP_MAKE_FLAGS="\$(DEFS) \$(INCLUDES) \$(ALL_CPPFLAGS)"
  else
    FPP_SOURCE_EXT="f"
    FPP_MAKE_FLAGS=""
fi
if test -z "$ac_fpp_out"; then
   FPP_OUTPUT=" "
else
   FPP_OUTPUT="> \[$]@"
fi
]) # _AC_FPP_BUILD_RULE


# -----------------------
# User macros (only one!) 
# -----------------------


# AC_PROG_FPP([required features])
# --------------------------------
#
# [required features] is a space-separated list of features that the Fortran
# preprocessor must have for the code to compile.
#
# It is up to the package maintainer to properly set these requirements.
#
# Supported features are:
#
# include   : correctly process #include directives and -I
# define    : correctly process -D
# substitute: substitute macros in Fortran code 
#             (some preprocessors touch only lines starting with #)
# wrap      : wrap lines that become too long through macro substitution  
#             fpp is probably the only preprocessor that does this.
# cstyle    : Do not suppress C style comments (-C option in cpp)
# CSTYLE    : *Do* suppress C style comments
#             (e.g. code contains C-style comments, and compiler may not
#             know how to handle them)
# 
# Features can be abbreviated: i, in, inc etc. are equivalent to include.
# Features can be deselected (feature not needed) by prepending "no", 
#   e.g. nodef (=nodefine), now (=nowrap).
#
# Default for the feature list is 
#       [include define substitute nowrap nocstyle noCSTYLE]
# Feature requirements corresponding to the defaults may be ommitted
#
# Note that "wrap" implies "substitute", and CSTYLE and cstyle cannot be requested
#   at the same time. The macro adjusts this automatically. 
#
# This macro sets and substitutes the variables FPP and FPPFLAGS, and also
# FPP_OUTPUT, FPP_MAKE_FLAGS, and FPP_SOURCE_EXT (see above)
#
# The macro depends on both F77 and CPP, because we must possibly fall 
# back on CPP for preprocessing.
#
AC_DEFUN([AC_PROG_FPP],
[AC_REQUIRE([AC_PROG_F77])dnl
AC_REQUIRE([AC_PROG_CPP])dnl
AC_ARG_VAR([FPP], [Command to preprocess Fortran 77 code])
AC_ARG_VAR([FPPFLAGS], [Flags for the Fortran 77 preprocessor])
_AC_PROG_FPP_FEATURES([$1])

# We first try to use F77 for compiling the source directly
# into object files
ac_ppf77_compile='${F77-f77} -c $FPPFLAGS $FFLAGS conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
ac_ppf77_link='${F77-f77} -o conftest${ac_exeext} $FPPFLAGS $FFLAGS $LDFLAGS conftest.$ac_ext $LIBS >&AS_MESSAGE_LOG_FD'

# _AC_PROG_F77_CPP stores results of the feature checks in non-cv variables,
# which we copy to cv variables afterwards.
# The reason for that is reusability of the macro for other cv variables (see below)
_AC_PROG_F77_CPP

AC_CACHE_CHECK([whether $F77 compiles programs with cpp directives], 
   ac_cv_prog_f77_cpp, 
  [ac_cv_prog_f77_cpp=$ac_prog_f77_cpp])

if test $ac_fpp_need_d = yes; then
  AC_CACHE_CHECK([whether $F77 accepts -D], 
     ac_cv_prog_f77_cpp_d, 
    [ac_cv_prog_f77_cpp_d=$ac_prog_f77_cpp_d])
fi

if test $ac_fpp_need_i = yes; then
  AC_CACHE_CHECK([whether $F77 accepts -I], 
     ac_cv_prog_f77_cpp_i,
    [ac_cv_prog_f77_cpp_i=$ac_prog_f77_cpp_i])
fi

if test $ac_fpp_need_subs = yes; then
  AC_CACHE_CHECK([whether $F77 substitutes macros in Fortran code], 
     ac_cv_prog_f77_cpp_subs,
    [ac_cv_prog_f77_cpp_subs=$ac_prog_f77_cpp_subs])
fi

if test $ac_fpp_need_wrap = yes; then 
  AC_CACHE_CHECK([whether $F77 wraps long lines automatically], 
     ac_cv_prog_f77_cpp_wrap,
    [ac_cv_prog_f77_cpp_wrap=$ac_prog_f77_cpp_wrap])
fi

if test $ac_fpp_need_CSTYLE = yes; then 
  AC_CACHE_CHECK([whether $F77 handles C-style comments], 
     ac_cv_prog_f77_cpp_CSTYLE,
    [ac_cv_prog_f77_cpp_CSTYLE=$ac_prog_f77_cpp_CSTYLE])
fi

AC_CACHE_CHECK([whether $F77 fulfills requested features],
  ac_cv_prog_f77_cpp_ok,
  [ac_cv_prog_f77_cpp_ok=$ac_fpp_ok])

# Now we check how to invoke a preprocessor that outputs Fortran code
# that F77 can understand
# The next macro sets FPP (unless already set by the user)
_AC_PROG_FPP
_AC_PROG_FPP_P

# Now, we check the features of the preprocessor/compiler combination
# It only makes sense to carry out further tests if FPP is a different
# program than F77
ac_fpp_name=`expr "x$FPP" : "x\([[^ ]]*\)"`
ac_f77_name=`expr "x$F77" : "x\([[^ ]]*\)"`
if test "x$ac_f77_name" != "x$ac_fpp_name"; then
  ac_fpp_equals_f77=no

# Redefine the compile and link commands for indirect compilation
  ac_ppf77_compile='${FPP-fpp} $FPPFLAGS conftest.$ac_ext '"$ac_fpp_out"' && ${F77-f77} -c $FFLAGS conftest.f >&AS_MESSAGE_LOG_FD'
  ac_ppf77_link='${FPP-fpp} $FPPFLAGS conftest.$ac_ext '"$ac_fpp_out"' && ${F77-f77} -o conftest${ac_exeext} $FFLAGS $LDFLAGS conftest.f $LIBS >&AS_MESSAGE_LOG_FD'

# Redo all the feature checks for indirect compilation: perhaps we have an
# external preprocessor that does a better job than F77
  _AC_PROG_F77_CPP

else
  ac_fpp_equals_f77=yes
fi

if test $ac_fpp_need_d = yes; then
  AC_CACHE_CHECK([whether $FPP accepts -D], 
     ac_cv_prog_fpp_d, 
    [ac_cv_prog_fpp_d=$ac_prog_f77_cpp_d])
fi

if test $ac_fpp_need_i = yes; then
  AC_CACHE_CHECK([whether $FPP accepts -I], 
     ac_cv_prog_fpp_i,
    [ac_cv_prog_fpp_i=$ac_prog_f77_cpp_i])
fi

if test $ac_fpp_need_subs = yes; then
  AC_CACHE_CHECK([whether $FPP substitutes macros in Fortran code], 
     ac_cv_prog_fpp_subs,
    [ac_cv_prog_fpp_subs=$ac_prog_f77_cpp_subs])
fi

if test $ac_fpp_need_wrap = yes; then 
  AC_CACHE_CHECK([whether $FPP wraps long lines automatically], 
     ac_cv_prog_fpp_wrap,
    [ac_cv_prog_fpp_wrap=$ac_prog_f77_cpp_wrap])
fi

if test $ac_fpp_need_CSTYLE = yes; then 
  AC_CACHE_CHECK([whether $FPP suppresses C-style comments], 
     ac_cv_prog_fpp_CSTYLE,
    [ac_cv_prog_fpp_CSTYLE=$ac_prog_f77_cpp_CSTYLE])

elif test $ac_fpp_need_cstyle = yes; then 
# It makes only sense to test this for indirect compilation, 
# i.e. if .f files are generated
    _AC_PROG_FPP_CSTYLE
fi

AC_CACHE_CHECK([whether $FPP fulfills requested features],
  ac_cv_prog_fpp_ok,
  [ac_cv_prog_fpp_ok=$ac_fpp_ok])

# We have all necessary information.
# It remains to construct optimal build rules 
# (direct: .fpp.o or indirect: .fpp.f)
# and carry out the substitutions
_AC_FPP_BUILD_RULE

AC_SUBST(FPP)
AC_SUBST(FPPFLAGS)
AC_SUBST(FPP_OUTPUT)
AC_SUBST(FPP_MAKE_FLAGS)
AC_SUBST(FPP_SOURCE_EXT) 

]) # AC_PROG_FPP
# --- FPP macros





## ------------------------------- ##
## 4. Compilers' characteristics.  ##
## ------------------------------- ##


# ---------------------------------------- #
# 4d. Fortran 77 compiler characteristics. #
# ---------------------------------------- #


# _AC_PROG_F77_V_OUTPUT([FLAG = $ac_cv_prog_f77_v])
# -------------------------------------------------
# Link a trivial Fortran program, compiling with a verbose output FLAG
# (which default value, $ac_cv_prog_f77_v, is computed by
# _AC_PROG_F77_V), and return the output in $ac_f77_v_output.  This
# output is processed in the way expected by AC_F77_LIBRARY_LDFLAGS,
# so that any link flags that are echoed by the compiler appear as
# space-separated items.
AC_DEFUN([_AC_PROG_F77_V_OUTPUT],
[AC_REQUIRE([AC_PROG_F77])dnl
AC_LANG_PUSH(Fortran 77)dnl

AC_LANG_CONFTEST([AC_LANG_PROGRAM([])])

# Compile and link our simple test program by passing a flag (argument
# 1 to this macro) to the Fortran 77 compiler in order to get
# "verbose" output that we can then parse for the Fortran 77 linker
# flags.
ac_save_FFLAGS=$FFLAGS
FFLAGS="$FFLAGS m4_default([$1], [$ac_cv_prog_f77_v])"
(eval echo $as_me:__oline__: \"$ac_link\") >&AS_MESSAGE_LOG_FD
ac_f77_v_output=`eval $ac_link AS_MESSAGE_LOG_FD>&1 2>&1 | grep -v 'Driving:'`
echo "$ac_f77_v_output" >&AS_MESSAGE_LOG_FD
FFLAGS=$ac_save_FFLAGS

rm -f conftest*
AC_LANG_POP(Fortran 77)dnl

# If we are using xlf then replace all the commas with spaces.
if echo $ac_f77_v_output | grep xlfentry >/dev/null 2>&1; then
  ac_f77_v_output=`echo $ac_f77_v_output | sed 's/,/ /g'`
fi

# On HP/UX there is a line like: "LPATH is: /foo:/bar:/baz" where
# /foo, /bar, and /baz are search directories for the Fortran linker.
# Here, we change these into -L/foo -L/bar -L/baz (and put it first):
ac_f77_v_output="`echo $ac_f77_v_output |
	grep 'LPATH is:' |
	sed 's,.*LPATH is\(: *[[^ ]]*\).*,\1,;s,: */, -L/,g'` $ac_f77_v_output"

# If we are using Cray Fortran then delete quotes.
# Use "\"" instead of '"' for font-lock-mode.
# FIXME: a more general fix for quoted arguments with spaces?
if echo $ac_f77_v_output | grep cft90 >/dev/null 2>&1; then
  ac_f77_v_output=`echo $ac_f77_v_output | sed "s/\"//g"`
fi[]dnl
])# _AC_PROG_F77_V_OUTPUT


# _AC_PROG_F77_V
# --------------
#
# Determine the flag that causes the Fortran 77 compiler to print
# information of library and object files (normally -v)
# Needed for AC_F77_LIBRARY_FLAGS
# Some compilers don't accept -v (Lahey: -verbose, xlf: -V, Fujitsu: -###)
AC_DEFUN([_AC_PROG_F77_V],
[AC_CACHE_CHECK([how to get verbose linking output from $F77],
                [ac_cv_prog_f77_v],
[AC_LANG_ASSERT(Fortran 77)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
[ac_cv_prog_f77_v=
# Try some options frequently used verbose output
for ac_verb in -v -verbose --verbose -V -\#\#\#; do
  _AC_PROG_F77_V_OUTPUT($ac_verb)
  # look for -l* and *.a constructs in the output
  for ac_arg in $ac_f77_v_output; do
     case $ac_arg in
        [[\\/]]*.a | ?:[[\\/]]*.a | -[[lLRu]]*)
          ac_cv_prog_f77_v=$ac_verb
          break 2 ;;
     esac
  done
done
if test -z "$ac_cv_prog_f77_v"; then
   AC_MSG_WARN([cannot determine how to obtain linking information from $F77])
fi],
                  [AC_MSG_WARN([compilation failed])])
])])# _AC_PROG_F77_V


# AC_F77_LIBRARY_LDFLAGS
# ----------------------
#
# Determine the linker flags (e.g. "-L" and "-l") for the Fortran 77
# intrinsic and run-time libraries that are required to successfully
# link a Fortran 77 program or shared library.  The output variable
# FLIBS is set to these flags.
#
# This macro is intended to be used in those situations when it is
# necessary to mix, e.g. C++ and Fortran 77, source code into a single
# program or shared library.
#
# For example, if object files from a C++ and Fortran 77 compiler must
# be linked together, then the C++ compiler/linker must be used for
# linking (since special C++-ish things need to happen at link time
# like calling global constructors, instantiating templates, enabling
# exception support, etc.).
#
# However, the Fortran 77 intrinsic and run-time libraries must be
# linked in as well, but the C++ compiler/linker doesn't know how to
# add these Fortran 77 libraries.  Hence, the macro
# "AC_F77_LIBRARY_LDFLAGS" was created to determine these Fortran 77
# libraries.
#
# This macro was packaged in its current form by Matthew D. Langston.
# However, nearly all of this macro came from the "OCTAVE_FLIBS" macro
# in "octave-2.0.13/aclocal.m4", and full credit should go to John
# W. Eaton for writing this extremely useful macro.  Thank you John.
AC_DEFUN([AC_F77_LIBRARY_LDFLAGS],
[AC_LANG_PUSH(Fortran 77)dnl
_AC_PROG_F77_V
AC_CACHE_CHECK([for Fortran 77 libraries], ac_cv_flibs,
[if test "x$FLIBS" != "x"; then
  ac_cv_flibs="$FLIBS" # Let the user override the test.
else

_AC_PROG_F77_V_OUTPUT

ac_cv_flibs=

# Save positional arguments (if any)
ac_save_positional="$[@]"

set X $ac_f77_v_output
while test $[@%:@] != 1; do
  shift
  ac_arg=$[1]
  case $ac_arg in
        [[\\/]]*.a | ?:[[\\/]]*.a)
          _AC_LIST_MEMBER_IF($ac_arg, $ac_cv_flibs, ,
              ac_cv_flibs="$ac_cv_flibs $ac_arg")
          ;;
        -bI:*)
          _AC_LIST_MEMBER_IF($ac_arg, $ac_cv_flibs, ,
             [_AC_LINKER_OPTION([$ac_arg], ac_cv_flibs)])
          ;;
          # Ignore these flags.
        -lang* | -lcrt0.o | -lc | -lgcc | -libmil | -LANG:=*)
          ;;
        -lkernel32)
          test x"$CYGWIN" != xyes && ac_cv_flibs="$ac_cv_flibs $ac_arg"
          ;;
        -[[LRuY]])
          # These flags, when seen by themselves, take an argument.
          # We remove the space between option and argument and re-iterate
          # unless we find an empty arg or a new option (starting with -)
	  case $[2] in
             "" | -*);;
             *)
		ac_arg="$ac_arg$[2]"
		shift; shift
		set X $ac_arg "$[@]"
		;;
	  esac
          ;;
        -YP,*)
          for ac_j in `echo $ac_arg | sed -e 's/-YP,/-L/;s/:/ -L/g'`; do
            _AC_LIST_MEMBER_IF($ac_j, $ac_cv_flibs, ,
                               [ac_arg="$ac_arg $ac_j"
                               ac_cv_flibs="$ac_cv_flibs $ac_j"])
          done
          ;;
        -[[lLR]]*)
          _AC_LIST_MEMBER_IF($ac_arg, $ac_cv_flibs, ,
                             ac_cv_flibs="$ac_cv_flibs $ac_arg")
          ;;
          # Ignore everything else.
  esac
done
# restore positional arguments
set X $ac_save_positional; shift

# We only consider "LD_RUN_PATH" on Solaris systems.  If this is seen,
# then we insist that the "run path" must be an absolute path (i.e. it
# must begin with a "/").
case `(uname -sr) 2>/dev/null` in
   "SunOS 5"*)
      ac_ld_run_path=`echo $ac_f77_v_output |
                        sed -n 's,^.*LD_RUN_PATH *= *\(/[[^ ]]*\).*$,-R\1,p'`
      test "x$ac_ld_run_path" != x &&
        _AC_LINKER_OPTION([$ac_ld_run_path], ac_cv_flibs)
      ;;
esac
fi # test "x$FLIBS" = "x"
])
FLIBS="$ac_cv_flibs"
AC_SUBST(FLIBS)
AC_LANG_POP(Fortran 77)dnl
])# AC_F77_LIBRARY_LDFLAGS


# AC_F77_DUMMY_MAIN([ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
# -----------------------------------------------------------
#
# Detect name of dummy main routine required by the Fortran libraries,
# (if any) and define F77_DUMMY_MAIN to this name (which should be
# used for a dummy declaration, if it is defined).  On some systems,
# linking a C program to the Fortran library does not work unless you
# supply a dummy function called something like MAIN__.
#
# Execute ACTION-IF-NOT-FOUND if no way of successfully linking a C
# program with the F77 libs is found; default to exiting with an error
# message.  Execute ACTION-IF-FOUND if a dummy routine name is needed
# and found or if it is not needed (default to defining F77_DUMMY_MAIN
# when needed).
#
# What is technically happening is that the Fortran libraries provide
# their own main() function, which usually initializes Fortran I/O and
# similar stuff, and then calls MAIN__, which is the entry point of
# your program.  Usually, a C program will override this with its own
# main() routine, but the linker sometimes complain if you don't
# provide a dummy (never-called) MAIN__ routine anyway.
#
# Of course, programs that want to allow Fortran subroutines to do
# I/O, etcetera, should call their main routine MAIN__() (or whatever)
# instead of main().  A separate autoconf test (AC_F77_MAIN) checks
# for the routine to use in this case (since the semantics of the test
# are slightly different).  To link to e.g. purely numerical
# libraries, this is normally not necessary, however, and most C/C++
# programs are reluctant to turn over so much control to Fortran.  =)
#
# The name variants we check for are (in order):
#   MAIN__ (g77, MAIN__ required on some systems; IRIX, MAIN__ optional)
#   MAIN_, __main (SunOS)
#   MAIN _MAIN __MAIN main_ main__ _main (we follow DDD and try these too)
AC_DEFUN([AC_F77_DUMMY_MAIN],
[AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])dnl
m4_define([_AC_LANG_PROGRAM_C_F77_HOOKS],
[#ifdef F77_DUMMY_MAIN
#  ifdef __cplusplus
     extern "C"
#  endif
   int F77_DUMMY_MAIN() { return 1; }
#endif
])
AC_CACHE_CHECK([for dummy main to link with Fortran 77 libraries],
               ac_cv_f77_dummy_main,
[AC_LANG_PUSH(C)dnl
 ac_f77_dm_save_LIBS=$LIBS
 LIBS="$LIBS $FLIBS"

 # First, try linking without a dummy main:
 AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
                [ac_cv_f77_dummy_main=none],
                [ac_cv_f77_dummy_main=unknown])

 if test $ac_cv_f77_dummy_main = unknown; then
   for ac_func in MAIN__ MAIN_ __main MAIN _MAIN __MAIN main_ main__ _main; do
     AC_LINK_IFELSE([AC_LANG_PROGRAM([[@%:@define F77_DUMMY_MAIN $ac_func]])],
                    [ac_cv_f77_dummy_main=$ac_func; break])
   done
 fi
 rm -f conftest*
 LIBS=$ac_f77_dm_save_LIBS
 AC_LANG_POP(C)dnl
])
F77_DUMMY_MAIN=$ac_cv_f77_dummy_main
AS_IF([test "$F77_DUMMY_MAIN" != unknown],
      [m4_default([$1],
[if test $F77_DUMMY_MAIN != none; then
  AC_DEFINE_UNQUOTED([F77_DUMMY_MAIN], $F77_DUMMY_MAIN,
                     [Define to dummy `main' function (if any) required to
                      link to the Fortran 77 libraries.])
fi])],
      [m4_default([$2],
            [AC_MSG_FAILURE([linking to Fortran libraries from C fails])])])
])# AC_F77_DUMMY_MAIN


# AC_F77_MAIN
# -----------
# Define F77_MAIN to name of alternate main() function for use with
# the Fortran libraries.  (Typically, the libraries may define their
# own main() to initialize I/O, etcetera, that then call your own
# routine called MAIN__ or whatever.)  See AC_F77_DUMMY_MAIN, above.
# If no such alternate name is found, just define F77_MAIN to main.
#
AC_DEFUN([AC_F77_MAIN],
[AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])dnl
AC_CACHE_CHECK([for alternate main to link with Fortran 77 libraries],
               ac_cv_f77_main,
[AC_LANG_PUSH(C)dnl
 ac_f77_m_save_LIBS=$LIBS
 LIBS="$LIBS $FLIBS"
 ac_cv_f77_main="main" # default entry point name

 for ac_func in MAIN__ MAIN_ __main MAIN _MAIN __MAIN main_ main__ _main; do
   AC_LINK_IFELSE([AC_LANG_PROGRAM([@%:@undef F77_DUMMY_MAIN
@%:@define main $ac_func])],
                  [ac_cv_f77_main=$ac_func; break])
 done
 rm -f conftest*
 LIBS=$ac_f77_m_save_LIBS
 AC_LANG_POP(C)dnl
])
AC_DEFINE_UNQUOTED([F77_MAIN], $ac_cv_f77_main,
                   [Define to alternate name for `main' routine that is
                    called from a `main' in the Fortran libraries.])
])# AC_F77_MAIN


# _AC_F77_NAME_MANGLING
# ---------------------
# Test for the name mangling scheme used by the Fortran 77 compiler.
#
# Sets ac_cv_f77_mangling. The value contains three fields, separated
# by commas:
#
# lower case / upper case:
#    case translation of the Fortran 77 symbols
# underscore / no underscore:
#    whether the compiler appends "_" to symbol names
# extra underscore / no extra underscore:
#    whether the compiler appends an extra "_" to symbol names already
#    containing at least one underscore
#
AC_DEFUN([_AC_F77_NAME_MANGLING],
[AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])dnl
AC_REQUIRE([AC_F77_DUMMY_MAIN])dnl
AC_CACHE_CHECK([for Fortran 77 name-mangling scheme],
               ac_cv_f77_mangling,
[AC_LANG_PUSH(Fortran 77)dnl
AC_COMPILE_IFELSE(
[      subroutine foobar()
      return
      end
      subroutine foo_bar()
      return
      end],
[mv conftest.$ac_objext cf77_test.$ac_objext

  AC_LANG_PUSH(C)dnl

  ac_save_LIBS=$LIBS
  LIBS="cf77_test.$ac_objext $LIBS $FLIBS"

  ac_success=no
  for ac_foobar in foobar FOOBAR; do
    for ac_underscore in "" "_"; do
      ac_func="$ac_foobar$ac_underscore"
      AC_LINK_IFELSE([AC_LANG_CALL([], [$ac_func])],
                     [ac_success=yes; break 2])
    done
  done

  if test "$ac_success" = "yes"; then
     case $ac_foobar in
        foobar)
           ac_case=lower
           ac_foo_bar=foo_bar
           ;;
        FOOBAR)
           ac_case=upper
           ac_foo_bar=FOO_BAR
           ;;
     esac

     ac_success_extra=no
     for ac_extra in "" "_"; do
        ac_func="$ac_foo_bar$ac_underscore$ac_extra"
        AC_LINK_IFELSE([AC_LANG_CALL([], [$ac_func])],
                       [ac_success_extra=yes; break])
     done

     if test "$ac_success_extra" = "yes"; then
	ac_cv_f77_mangling="$ac_case case"
        if test -z "$ac_underscore"; then
           ac_cv_f77_mangling="$ac_cv_f77_mangling, no underscore"
	else
           ac_cv_f77_mangling="$ac_cv_f77_mangling, underscore"
        fi
        if test -z "$ac_extra"; then
           ac_cv_f77_mangling="$ac_cv_f77_mangling, no extra underscore"
	else
           ac_cv_f77_mangling="$ac_cv_f77_mangling, extra underscore"
        fi
      else
	ac_cv_f77_mangling="unknown"
      fi
  else
     ac_cv_f77_mangling="unknown"
  fi

  LIBS=$ac_save_LIBS
  AC_LANG_POP(C)dnl
  rm -f cf77_test* conftest*],
  [AC_MSG_FAILURE([cannot compile a simple Fortran program])])
AC_LANG_POP(Fortran 77)dnl
])
])# _AC_F77_NAME_MANGLING

# The replacement is empty.
AU_DEFUN([AC_F77_NAME_MANGLING], [])


# AC_F77_WRAPPERS
# ---------------
# Defines C macros F77_FUNC(name,NAME) and F77_FUNC_(name,NAME) to
# properly mangle the names of C identifiers, and C identifiers with
# underscores, respectively, so that they match the name mangling
# scheme used by the Fortran 77 compiler.
AC_DEFUN([AC_F77_WRAPPERS],
[AC_REQUIRE([_AC_F77_NAME_MANGLING])dnl
AH_TEMPLATE([F77_FUNC],
    [Define to a macro mangling the given C identifier (in lower and upper
     case), which must not contain underscores, for linking with Fortran.])dnl
AH_TEMPLATE([F77_FUNC_],
    [As F77_FUNC, but for C identifiers containing underscores.])dnl
case $ac_cv_f77_mangling in
  "lower case, no underscore, no extra underscore")
          AC_DEFINE([F77_FUNC(name,NAME)],  [name])
          AC_DEFINE([F77_FUNC_(name,NAME)], [name]) ;;
  "lower case, no underscore, extra underscore")
          AC_DEFINE([F77_FUNC(name,NAME)],  [name])
          AC_DEFINE([F77_FUNC_(name,NAME)], [name ## _]) ;;
  "lower case, underscore, no extra underscore")
          AC_DEFINE([F77_FUNC(name,NAME)],  [name ## _])
          AC_DEFINE([F77_FUNC_(name,NAME)], [name ## _]) ;;
  "lower case, underscore, extra underscore")
          AC_DEFINE([F77_FUNC(name,NAME)],  [name ## _])
          AC_DEFINE([F77_FUNC_(name,NAME)], [name ## __]) ;;
  "upper case, no underscore, no extra underscore")
          AC_DEFINE([F77_FUNC(name,NAME)],  [NAME])
          AC_DEFINE([F77_FUNC_(name,NAME)], [NAME]) ;;
  "upper case, no underscore, extra underscore")
          AC_DEFINE([F77_FUNC(name,NAME)],  [NAME])
          AC_DEFINE([F77_FUNC_(name,NAME)], [NAME ## _]) ;;
  "upper case, underscore, no extra underscore")
          AC_DEFINE([F77_FUNC(name,NAME)],  [NAME ## _])
          AC_DEFINE([F77_FUNC_(name,NAME)], [NAME ## _]) ;;
  "upper case, underscore, extra underscore")
          AC_DEFINE([F77_FUNC(name,NAME)],  [NAME ## _])
          AC_DEFINE([F77_FUNC_(name,NAME)], [NAME ## __]) ;;
  *)
          AC_MSG_WARN([unknown Fortran 77 name-mangling scheme])
          ;;
esac
])# AC_F77_WRAPPERS


# AC_F77_FUNC(NAME, [SHELLVAR = NAME])
# ------------------------------------
# For a Fortran subroutine of given NAME, define a shell variable
# $SHELLVAR to the Fortran-77 mangled name.  If the SHELLVAR
# argument is not supplied, it defaults to NAME.
AC_DEFUN([AC_F77_FUNC],
[AC_REQUIRE([_AC_F77_NAME_MANGLING])dnl
case $ac_cv_f77_mangling in
  upper*) ac_val="m4_toupper([$1])" ;;
  lower*) ac_val="m4_tolower([$1])" ;;
  *)      ac_val="unknown" ;;
esac
case $ac_cv_f77_mangling in *," underscore"*) ac_val="$ac_val"_ ;; esac
m4_if(m4_index([$1],[_]),-1,[],
[case $ac_cv_f77_mangling in *," extra underscore"*) ac_val="$ac_val"_ ;; esac
])
m4_default([$2],[$1])="$ac_val"
])# AC_F77_FUNC
