dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(fio, 1.5-3, ussc@star.rl.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require automake-1.6 at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([fio1_open.f])

dnl   Include defaults for Starlink configurations
STAR_DEFAULTS

dnl   Checks for programs
AC_PROG_F77
AC_PROG_LIBTOOL

STAR_DECLARE_BUILD_DEPENDENCIES(sae par)
STAR_DECLARE_USE_DEPENDENCIES(chr ems mers subpar)

dnl   Documentation is uncomplicated
STAR_LATEX_DOCUMENTATION(sun143)

dnl   Declare options for platform-specific sources.
dnl   The original set of extensions was [alpha_OSF1 ix86_Linux mips
dnl   sun4 sun4_Solaris vms], which has migrated to [_dec_osf
dnl   _pc_linux _sun_solaris _sun mips vax].  The `vax' option is
dnl   actually the old VMS option -- which is redundant in some sense,
dnl   since VMS not a Unix (gosh): perhaps this would be better as __ultrix.
STAR_PLATFORM_SOURCES([fio1_open.f],
                      [_dec_osf _pc_linux _sun_solaris _sun mips vax])
STAR_PLATFORM_SOURCES([fio1_serr.f],
                      [_dec_osf _pc_linux _sun_solaris _sun mips vax])
STAR_PLATFORM_SOURCES([rio1_open.f],
                      [_dec_osf _pc_linux _sun_solaris _sun mips vax])
dnl   The sun options require a sun_iosdef file, but if neither of these
dnl   two matches, that's OK -- presumably we're not on a Sun.
STAR_PLATFORM_SOURCES([sun_iosdef], [_sun_solaris _sun NONE])

STAR_MESSGEN(fio_err.msg)

dnl   Header checks.  There are no such explicit checks at present,
dnl   only the implicit ones.  Also config.h is not at present
dnl   included by anything.  However we might as well include this
dnl   stanza here, to support that in future, but also to keep down
dnl   the length of generated command lines.
AM_CONFIG_HEADER(config.h)


AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
