dnl  Autoconf script for configuring the Makefile for the
dnl  SC/13 demo programs.

dnl  This autoconf script could do with some modernisation.  In
dnl  particular, the support for -fno-underscoring and the like should
dnl  be overhauled; I should possibly use AC_F77_WRAPPERS instead.
dnl  However, the fact that the programs here are intended to be to some
dnl  extent educational might mean that we can sensibly expose more of
dnl  this than we would otherwise want to.

AC_INIT(sc13-examples, 1.0, norman@astro.gla.ac.uk)

AM_INIT_AUTOMAKE

AC_CONFIG_SRCDIR(fpp.c)

AC_PROG_CC
AC_PROG_FC

AC_CHECK_HEADERS([errno.h])

dnl  What options do we need to pass to the compiler to get it to
dnl  include profiling code?  Allow this to be specified with
dnl  --with-profiling=<option>.  If that isn't present, then look
dnl  for the program gprof in the path, and set profilingoption to -pg 
dnl  if we find it.
AC_SUBST(PROFILINGOPTION)
PROFILINGOPTION=

AC_SUBST(NOUNDERSCORES)
NOUNDERSCORES=

AC_SUBST(ENABLENAN)
ENABLENAN=

dnl  Is gprof available?  If no value is given, see if we can find
dnl  gprof, and if so, set PROFILINGOPTION to -pg; otherwise, set it
dnl  to ""
AC_ARG_ENABLE(profiling,
[  --enable-profiling=opt  Give the compiler option to include profiling code],
  PROFILINGOPTION=$enableval,
  AC_CHECK_PROG(PROFILINGOPTION, gprof, -pg, ""))

dnl  If --enable-nounderscore is not present, set it to ""
AC_ARG_ENABLE(nounderscore,
[  --enable-nounderscore=opt  
                          Give the compiler option to suppress
                          trailing underscores in symbol names],
  NOUNDERSCORES=$enableval)

AC_ARG_ENABLE(nan,
[  --enable-nan=opt       C-compiler switch to suppress trap on IEEE exceptions],
  ENABLENAN=$enableval)


dnl  We know what these options are for gcc/g77, and autoconf makes it
dnl  easy to test whether it's gcc/g77 we're using, so special-case
dnl  these:
dnl  HACK HACK HACK: this is not the way to do this -- I should test
dnl  the various known options and keep those which work!
if test "$NOUNDERSCORES" != "no"; then
  if test "$G77" = "yes"; then
    if test -z "$PROFILINGOPTION"; then
      PROFILINGOPTION='-pg'
    fi
    if test -z "$NOUNDERSCORES"; then
      NOUNDERSCORES="-fno-underscoring"
    fi
  fi
fi

dnl  echo "ac_cv_fc_compiler_gnu=$ac_cv_fc_compiler_gnu  NOUNDERSCORES=$NOUNDERSCORES  PROFILINGOPTION=$PROFILINGOPTION"

dnl  If we haven't set both of PROFILINGOPTION and NOUNDERSCORES by
dnl  now, then try to guess appropriate values based on the host
dnl  platform.  This isn't ideal, since it can't tell if you're using
dnl  a non-default compiler, but we'll try to use the options at the
dnl  end, and try to give a helpful error message if it doesn't work.
dnl
dnl  Both gcc and Sun cc permit IEEE exceptional values by default,
dnl  Alpha cc traps on exceptional values.
if test -z "$PROFILINGOPTION" -o -z "$NOUNDERSCORES"; then
    AC_CANONICAL_HOST
    echo "Target system $host"
    case "$host" in
      sparc-*-*) 
        if test -z "$PROFILINGOPTION" -o "$PROFILINGOPTION" != "no"; then 
            PROFILINGOPTION=-pg
        fi
        if test -z "$NOUNDERSCORES" -o "$NOUNDERSCORES" != "no"; then
            NOUNDERSCORES=-ext_names=plain
        fi
        ;;
      alpha*)
        if test -z "$PROFILINGOPTION" -o "$PROFILINGOPTION" != "no"; then 
            PROFILINGOPTION=-pg
        fi
        if test -z "$NOUNDERSCORES" -o "$NOUNDERSCORES" != "no"; then
            NOUNDERSCORES='-assume nounderscore'
        fi
        if test -z "$ENABLENAN" -o "$ENABLENAN" != "no"; then
            ENABLENAN=-ieee
        fi
        ;;
      *)
        AC_MSG_WARN([
  I can't work out how to specify the profiling,
  no-underscore and enable-nan options for your C/Fortran compilers.
  You can specify them with the --enable-profiling=<option>,
  --enable-nounderscore=<option>, and --enable-nan=<option> configure
  option.  If you have to do this, can you mail norman@astro.gla.ac.uk
  with the appropriate options, quoting the compiler type
      $host.
  Thanks.])
         ;;
    esac
fi

dnl  Now test the options we have set up, by compiling a simple
dnl  Fortran program
AC_LANG_PUSH([Fortran 77])
dnl   Following triggers a bug in autoconf 2.13 (reported to
dnl  http://sources.redhat.com/autoconf/).
dnl
dnl safe_FFLAGS=$FFLAGS
dnl FFLAGS="FFLAGS $PROFILINGOPTION $NOUNDERSCORES"
dnl AC_TRY_COMPILE(,
dnl       I=1,
dnl   echo "Fortran flags $FFLAGS work",
dnl   [AC_MSG_WARN([
dnl   Oooh-err.  Based on your platform, $host, I thought that the Fortran
dnl   compiler options $FFLAGS would work, but they don't appear to.
dnl   Perhaps the Fortran compiler you've selected, $F77, isn't the default
dnl   one for this platform.  You can force the profiling options and
dnl   no-underscores options by using the configure flags
dnl    --enable-profiling=<option> and --enable-nounderscore=<option>])])
dnl FFLAGS=$safe_FFLAGS

AC_MSG_CHECKING([whether fortran options are ok])
foptsok=false
safe_FFLAGS=$FFLAGS
FFLAGS="$FFLAGS $PROFILINGOPTION $NOUNDERSCORES $ENABLENAN"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [
      I=1
])],
                  foptsok=true,
                  foptsok=false)
if $foptsok; then
    AC_MSG_RESULT([ok])
else
    AC_MSG_FAILURE([NO])
    echo "Oooh-err.  Based on your platform, $host"
    echo "I thought that the Fortran compiler options "
    echo "    $F77 $FFLAGS $PROFILINGOPTION $NOUNDERSCORES $ENABLENAN -c conftest.f"
    echo "would work, but they don't appear to.  Perhaps the Fortran"
    echo "compiler you've selected, $F77, isn't the default one for this"
    echo "platform.  You can supply the compiler options yourself"
    echo "by using the configure flags --enable-profiling=<option>"
    echo "--enable-nounderscore=<option>, --enable-nan=<option>"
fi
FFLAGS=$safe_FFLAGS

dnl  dnl  So do it by hand.
dnl  cat >conftest.f <<EOF
dnl        PROGRAM CONFTEST
dnl        I=1
dnl        END
dnl  EOF
dnl  $FC $FFLAGS $PROFILINGOPTION $NOUNDERSCORES $ENABLENAN -c conftest.f
dnl  if test $? = 0; then
dnl      echo "Fortran flags $FFLAGS $PROFILINGOPTION $NOUNDERSCORES $ENABLENAN work"
dnl  else
dnl      AC_MSG_WARN([
dnl      Oooh-err.  Based on your platform, 
dnl          $host
dnl      I thought that the Fortran compiler options 
dnl          $FC $FFLAGS $PROFILINGOPTION $NOUNDERSCORES $ENABLENAN -c conftest.f
dnl      would work, but they don't appear to.  Perhaps the Fortran
dnl      compiler you've selected, $F77, isn't the default one for this
dnl      platform.  You can supply the compiler options yourself
dnl      by using the configure flags --enable-profiling=<option>
dnl      --enable-nounderscore=<option>, --enable-nan=<option>])
dnl  fi
dnl  rm -f conftest.*

AC_CONFIG_FILES(Makefile)
dnl AC_CONFIG_FILES([stamp-h], [echo timestamp > stamp-h])

AC_CONFIG_HEADERS(config.h)
CFLAGS="$CFLAGS -I."

AC_OUTPUT
