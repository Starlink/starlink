dnl  Autoconf file for ASTROM
dnl  $Id$

AC_INIT(astrom, 3.7-1, ussc@starlink.ac.uk)

dnl  Everything depends on where /star is.  Declare STARLINK as a
dnl  `precious variable'
AC_ARG_VAR(STARLINK, [Where all the Starlink software lives])

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

dnl   Check whether we need to include `-lnsl -lsocket' to use the
dnl   socket() call and friends (the t/makefile needs this, and the
dnl  moggy configuration will use this also)
AC_CHECK_FUNC(gethostbyname,,
  AC_CHECK_LIB(nsl, gethostbyname))
AC_CHECK_FUNC(socket,,
  AC_CHECK_LIB(socket, socket,,,-lnsl))

dnl  This is a rather unsatisfactory way of helping Fortran programs
dnl  link to C ones.  Better is the AC_F77_WRAPPERS macro, but that
dnl  requires cooperation on the part of the C library in question
dnl  (actually slalib, here)
if test "$F77" = g77; then
  FFLAGS="-fno-second-underscore $FFLAGS"
fi

dnl  Set the default WCS style.  This can be overridden by setting the
dnl  environment variable ASTROMWCSSTYLE to one of the legal ASTROM
dnl  header styles, or by setting that variable in the ./configure argument
dnl  list (see the documentation).  This is, at present, primarily
dnl  intended to be used by other programs configuring this one.
AC_ARG_VAR(ASTROMWCSSTYLE,
           [Default style ASTROM uses for output FITS WCS info])
if test -z "$ASTROMWCSSTYLE"; then
    ASTROMWCSSTYLE=qtan
fi

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
