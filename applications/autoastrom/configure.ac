dnl  Configure script for autoastrom
dnl  Copyright 2003 Council for the Central Laboratories of the 
dnl      Research Councils (`CCLRC').


AC_REVISION($Revision$)

dnl  package name, version, and bug-report address
AC_INIT(autoastrom, 0.5-12, [norman@astro.gla.ac.uk])

dnl  config.* and install-sh are in config/
AC_CONFIG_AUX_DIR(config)

dnl  Everything depends on where /star is.  Declare STARLINK as a
dnl  `precious variable'.  Amongst other things, this will make
dnl  ./configure squeal if the package is re-configured with an
dnl  inconsistent value of this variable.
AC_ARG_VAR(STARLINK, [Where all the Starlink software lives])

dnl  ...and make sure it does have a value
test "${STARLINK+set}" = set || STARLINK=/star

dnl  Install in /star by default (this is redundant at present, since
dnl  the makefile handles its own installation)
AC_PREFIX_DEFAULT(/star)

dnl  Search for Perl.  We want to find Starlink perl.
AC_PATH_PROG(STARPERL, perl, [/usr/bin/env perl], 
             [$STARLINK/Perl/bin:/stardev/Perl/bin:/star/Perl/bin:$PATH])

dnl  Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_F77
AC_PROG_RANLIB
AC_PROG_LN_S

dnl   Check whether we need to include `-lnsl -lsocket' to use the
dnl   socket() call and friends (the t/makefile needs this, and the
dnl  moggy configuration will use this also)
AC_CHECK_FUNC(gethostbyname,,
  AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(socket,,
  AC_CHECK_LIB(socket, socket,,,-lnsl))

dnl  Check for tar.  Set TAR_IN and TAR_OUT depending on whether we do
dnl  find tar, or whether we're stuck with POSIX pax.
AC_CHECK_PROG(TAR, tar, tar, pax)
if test "$TAR" = tar; then
    AC_SUBST(TAR_IN,  ["tar -cvhf"])
    AC_SUBST(TAR_OUT, ["tar -xf"])
else
    AC_SUBST(TAR_IN,  ["pax -w -v -x ustar -f"])
    AC_SUBST(TAR_OUT, ["pax -r -f"])
fi

dnl  Find the SYSTEM type.  If this is specified in the environment,
dnl  then use that value; otherwise, use AC_CANONICAL_BUILD.
AC_SUBST(BUILD_SYSTEM)
if test -n "$SYSTEM"; then
  BUILD_SYSTEM=$SYSTEM
else
  AC_CANONICAL_BUILD
  case "$build" in
    alpha*)                 BUILD_SYSTEM=alpha_OSF1 ;;
    ix86*|i486*|i686*)      BUILD_SYSTEM=ix86_Linux ;;
    sparc*)                 BUILD_SYSTEM=sun4_Solaris ;;
    *)
        echo "Warning: unable to recognise system type $host; using UNKNOWN"
        BUILD_SYSTEM=UNKNOWN
        ;;
  esac
fi

dnl  This is a rather unsatisfactory way of helping Fortran programs
dnl  link to C ones.  Better is the AC_F77_WRAPPERS macro, but that
dnl  requires co-operation on the part of the C library in question
dnl  (actually slalib, here)
if test "$F77" = g77; then
  FFLAGS="-fno-second-underscore $FFLAGS"
fi

dnl  This looks like a good place to run
dnl  AC_CONFIG_SUBDIRS(moggy astrom catlib).
dnl  Don't, however, since the autoastrom makefile configures each of
dnl  these three directories with options or variable settings which
dnl  it determines on the fly.  These could very probably be rationalised

dnl   That's all, at present
AC_OUTPUT(makefile t/makefile)
