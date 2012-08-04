dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(jpeg, 6b, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_MAKE_SET

dnl Check for the presence of an existing JPEG library. If it's available,
dnl then we don't have to do anything, and we set MAIN_TARGET to be
dnl jpeglib-stamp, which the Makefile generates trivially.  If it's not
dnl available, then we need to build jpeglib, so drop into the jpeg/
dnl directory and configure the software. Note this test did just check
dnl for jpeglib.h, but that doesn't work when only the compilation fails
dnl (which it may when using special CFLAGS, i.e. -m32 when only 64 bit
dnl libraries are available). Use jpeg_write_scanlines as the more obvious
dnl entry functions are really defines.
AC_CHECK_LIB([jpeg], [jpeg_write_scanlines],
  [MAIN_TARGET=jpeglib-stamp],
  [
  # To configure jpeg, run ./configure in the source directory.
  # Do not invoke AC_CONFIG_SUBDIRS.
  (
    cd jpeg
    echo ./configure --prefix=$prefix --cache-file=config.cache
    ./configure --prefix=$prefix --cache-file=config.cache
  )
  MAIN_TARGET=jpeglib
])
AC_SUBST(MAIN_TARGET)

dnl  The special install command is processed at a different time from
dnl  the rest of the script, so can't be skipped within the header
dnl  check above.  It thus has to be written to respect the result of
dnl  the test above.  Also, the jpeg/makefile.cfg doesn't create its
dnl  installation directories, so we have to do that by hand.
STAR_SPECIAL_INSTALL_COMMAND([
    if test $(MAIN_TARGET) = jpeglib; then
      cd jpeg;
      for d in bin lib include man/man1;
        do $(mkdir_p) $$DESTDIR$(prefix)/$$d;
      done;
      $(MAKE) DESTDIR=$$DESTDIR install install-lib;
    fi])


AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
