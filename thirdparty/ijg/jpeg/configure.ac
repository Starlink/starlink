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

dnl    To configure jpeg, run ./configure in the source directory.
dnl    Do not invoke AC_CONFIG_SUBDIRS.
(
    cd jpeg-6b
    echo ./configure --prefix=$prefix --cache-file=config.cache
    ./configure --prefix=$prefix --cache-file=config.cache
)

STAR_SPECIAL_INSTALL_COMMAND([
    cd jpeg-6b;
    $(MAKE) INSTALL_ROOT=$$DESTDIR install])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
