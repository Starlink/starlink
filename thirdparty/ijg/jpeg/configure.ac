dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(jpeg, 6b, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_MAKE_SET

# Check for the presence of the jpeglib.h file.  If it's available,
# then we don't have to do anything, and we set MAIN_TARGET to be
# jpeglib-stamp, which the Makefile generates trivially.  If it's not
# available, then we need to build jpeglib, so drop into the src/
# directory and configure the software.
AC_CHECK_HEADER([jpeglib.h],
  [MAIN_TARGET=jpeglib-stamp],
  [
  # To configure jpeg, run ./configure in the source directory.
  # Do not invoke AC_CONFIG_SUBDIRS.
  (
    cd src
    echo ./configure --prefix=$prefix --cache-file=config.cache
    ./configure --prefix=$prefix --cache-file=config.cache
  )
  MAIN_TARGET=jpeglib
])
AC_SUBST(MAIN_TARGET)

dnl  The special install command is processed at a different time from
dnl  the rest of the script, so can't be skipped within the header
dnl  check above.  It thus has to be written to respect the result of
dnl  the test above.  Also, the src/makefile.cfg doesn't create its
dnl  installation directories, so we have to do that by hand.
STAR_SPECIAL_INSTALL_COMMAND([
    if test $(MAIN_TARGET) = jpeglib; then
      cd src;
      for d in bin lib include man/man1;
        do $(mkdir_p) $$DESTDIR$(prefix)/$$d;
      done;
      $(MAKE) DESTDIR=$$DESTDIR install;
    fi])


AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
