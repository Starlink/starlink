dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(convert-conspp, 1.5-4, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([nlines.f])
 
dnl    Include defaults for Starlink configurations
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    The system we're building for determines if we have IRAF data support
dnl    as this is only available as pre-built static object libraries. Make
dnl    links to various forms of these libraries. Make sure the various
dnl    libraries do not exist as they are not removed on unsupported platforms.
dnl    The libcompat.a file is actually only needed for Linux. To link
dnl    against the selected libraries use the "iraf_link" script in the
dnl    convert directory. This determines which libraries are available.
dnl
dnl    In practice this code needs to be run by the convert configure script
dnl    (since that is executed before this, so any consequences of this code
dnl    are not seen), so it has been moved up.
dnl
dnl IRAFSUPPORT="libcompat.a libimfort.a libos.a libsys.a libvops.a"
dnl for f in $IRAFSUPPORT; do
dnl   if test -f "libimfort.a"; then
dnl     rm $f
dnl   fi
dnl done
dnl STAR_PLATFORM_SOURCES([$IRAFSUPPORT], 
dnl        [_pc_linux_gnu _sun_solaris _dec_osf _mips _apple_darwin NONE])

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
