dnl  Autoconf file for ASTROM
dnl  $Id$

AC_INIT(astrom, 3.7, ussc@starlink.ac.uk)

dnl  By default, we want to be installed into /star (this option has
dnl  no effect at present, since this is not how we currently
dnl  configure the installation directory in Starlink makefiles, but
dnl  it's here for future reference)
AC_PREFIX_DEFAULT(/star)

dnl  Where do the helper files live?
AC_CONFIG_AUX_DIR(config)

dnl  Name of the C compiler
AC_PROG_CC

dnl  Name of the Fortran compiler
AC_PROG_F77
dnl  Specify the fortran unit numbers for standard input and output.
dnl  We should test for this, but (a) I'm not sure how I'd write such
dnl  a test, and (b) they really are pretty standard, and this is just
dnl  covering my back.
AC_SUBST(FUNITIN, 5)
AC_SUBST(FUNITOUT, 6)

dnl  Work out how to link against Fortran libraries using a C compiler
AC_F77_LIBRARY_LDFLAGS
AC_F77_DUMMY_MAIN

dnl  Does ln -s behave?
AC_PROG_LN_S

dnl  Platform-specific stuff
AC_CANONICAL_BUILD
echo build=$build  build_cpu=$build_cpu  build_vendor=$build_vendor  build_os=$build_os
case $build_vendor in
    sun) LDFLAGS="$LDFLAGS -lnsl -lsocket"
        ;;
esac


dnl  Set the default WCS style.  This can be overridden by setting the
dnl  environment variable DEFWCSSTYLE to one of the legal ASTROM
dnl  header styles (see the documentation).  This is, at present,
dnl  intended only to be used by other programs configuring this one,
dnl  so it shouldn't be regarded as a public interface (that is, it
dnl  might change without warning).
if test -z "$DEFWCSSTYLE"; then
    DEFWCSSTYLE=qtan
fi
AC_SUBST(DEFWCSSTYLE)

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

AC_CONFIG_FILES(makefile params.f t/Makefile)
AC_OUTPUT
