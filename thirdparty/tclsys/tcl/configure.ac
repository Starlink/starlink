dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(tcl, 8.2.3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Get EXEEXT for make.config

AC_PROG_MAKE_SET

dnl    To configure Tcl, run ./configure in the `unix' directory.
dnl    Do not invoke AC_CONFIG_SUBDIRS, since that prompts autoreconf
dnl    to try to reconfigure that directory, and automake to assume 
dnl    it's allowed to play there, too.
(
    cd unix
    echo ./configure --prefix=$prefix --cache-file=config.cache
    ./configure --prefix=$prefix --cache-file=config.cache
)

STAR_SPECIAL_INSTALL_COMMAND([
    cd unix;
    $(MAKE) INSTALL_ROOT=$$DESTDIR install])

AC_CONFIG_FILES([Makefile component.xml make.config])

AC_OUTPUT
