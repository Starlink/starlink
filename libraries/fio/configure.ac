dnl  Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl   Initialisation: package name and version number
AC_INIT(fio, 1.5-4, starlink@jiscmail.ac.uk)

dnl   Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl   Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl   Sanity-check: name a file in the source directory
AC_CONFIG_SRCDIR([fio_link])

#   Include defaults for Starlink configurations
STAR_DEFAULTS

#   Checks for programs
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

AC_CHECK_HEADERS(errno.h)
AC_CHECK_HEADERS(time.h)
AC_CHECK_HEADERS(stdbool.h)
AC_CHECK_HEADERS(string.h)

AC_CHECK_FUNCS([strerror sys_nerr])

STAR_CNF_COMPATIBLE_SYMBOLS

#   Test what features the OPEN command can support
AC_FC_OPEN_SPECIFIERS(readonly)
AC_FC_OPEN_SPECIFIERS(access='append')
AC_FC_OPEN_SPECIFIERS(position='append')
AC_FC_OPEN_SPECIFIERS(blocksize=0)
AC_FC_OPEN_SPECIFIERS(organization='RELATIVE')
AC_FC_OPEN_SPECIFIERS(carriagecontrol='NONE')
AC_FC_OPEN_SPECIFIERS([access='sequential',recl=1])

#   What unit does OPEN's recl specifier assume?
AC_FC_RECL_UNIT

#   Tests require access to monoliths
#   This may be a problem if alink has not yet been installed
STAR_MONOLITHS

STAR_DECLARE_DEPENDENCIES([build], [par sae])
STAR_DECLARE_DEPENDENCIES([link], [chr ems mers pcs])

#  Documentation is uncomplicated
STAR_LATEX_DOCUMENTATION(sun143)

#   Declare options for platform-specific sources.
#   The original set of extensions was [alpha_OSF1 ix86_Linux mips
#   sun4 sun4_Solaris vms], which has migrated to [_dec_osf
#   _pc_linux _sun_solaris _sun mips vax].  The `vax' option is
#   actually the old VMS option -- which is redundant in some sense,
#   since VMS not a Unix (gosh): perhaps this would be better as __ultrix.
#
#   Note that fio1_open.f was originally handled this way, but has
#   been replaced by fio1_open.fpp, and fio1_open.f{_dec_osf,
#   _pc_linux, _sun, _sun_solaris, default, and mips} have been
#   deleted.  fio1_open.fvax has been retained, but this is largely
#   for historical interest.  Exactly the same for rio1_open.f ->
#   rio1_open.fpp and rio1_open.fvax.
#
#   fio1_serr.f has significant platform dependencies, and
#   can't be easily handled through this sort of configuration.
STAR_PLATFORM_SOURCES([fio1_serr.f],
                      [_dec_osf _pc_linux _sun_solaris _sun mips vax _apple_darwin _pc_cygwin _linux_gnu _pc_solaris])
#   The sun options require a sun_iosdef file, but if neither of these
#   two matches, that's OK -- presumably we're not on a Sun.
STAR_PLATFORM_SOURCES([sun_iosdef], [_sun_solaris _sun _pc_solaris NONE])

STAR_MESSGEN(fio_err.msg)

#   Header checks.  There are no such explicit checks at present,
#   only the implicit ones.  Also config.h is not at present
#   included by anything.  However we might as well include this
#   stanza here, to support that in future, but also to keep down
#   the length of generated command lines.
AM_CONFIG_HEADER(config.h)


AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
