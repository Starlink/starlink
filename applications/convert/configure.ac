dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(convert, 1.5-4, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([convert.f])

dnl    Main programs etc. go into convert sub-directory of bindir.
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

dnl    Setup for building monoliths.
STAR_MONOLITHS

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [figaro ast sae chr hds fio cnf grp mers prm ndf ndg par psx subpar])

dnl    Progs needed for gif2ndf, ndf2gif and tiff2ndf
STAR_CHECK_PROGS(ppmtogif)
STAR_CHECK_PROGS(giftopnm)
STAR_CHECK_PROGS(ppmtopgm)
STAR_CHECK_PROGS(pnmflip)
STAR_CHECK_PROGS(pnmtofits)
STAR_CHECK_PROGS(tifftopnm)

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    Documentation.
STAR_LATEX_DOCUMENTATION([sun55])

dnl    Library/component sub-directories.
AC_CONFIG_SUBDIRS([libaif libcof libcoi libcon idl iraf])

dnl    Check if we have any IRAF libraries on this platform.
dnl    On success this section gives a -DHAVE_IRAF_LIBIMFORT_A=1 macro
dnl    definition and substitutes @IRAFLIBS@ in the Makefile.am/.in.
dnl    We cd into iraf, rather than perform this test there as this configure
dnl    script is ran first.
(cd iraf
if test -f "libimfort.a"; then
  rm libimfort.a
fi
STAR_PLATFORM_SOURCES([libcompat.a libimfort.a libos.a libsys.a libvops.a],
        [_pc_linux_gnu _sun_solaris _dec_osf _mips _apple_darwin NONE]))

AC_CHECK_FILES([iraf/libimfort.a],
 [IRAFLIBS="iraf/libimfort.a iraf/libsys.a iraf/libvops.a iraf/libos.a iraf/libcompat.a"],
 [IRAFLIBS=])
AC_SUBST(IRAFLIBS)

dnl    Configure .in files.
AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
