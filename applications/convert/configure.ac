dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([convert],[1.8],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
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
LT_INIT

dnl    Setup for building monoliths.
STAR_MONOLITHS

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], 
 [ast chr cnf cvg cfitsio figaro fio grp hds kaplibs mers ndf ndg par pcs perl prm psx sae shl])

dnl    Need to pre-process generic code.
STAR_CHECK_PROGS(fgeneric)

dnl    Progs needed for gif2ndf, ndf2gif and tiff2ndf
STAR_CHECK_PROGS(ppmtogif)
STAR_CHECK_PROGS(giftopnm)
STAR_CHECK_PROGS(ppmtopgm)
STAR_CHECK_PROGS(pnmflip)
STAR_CHECK_PROGS(pnmtofits)
STAR_CHECK_PROGS(tifftopnm)

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)
STAR_CHECK_PROGS(prohlp,sst)

dnl    Ranlib is needed on OS X so that we can update the static libraries
dnl    (the archive index is considered out of date if it doesn't match the
dnl    date of the archive file, so a simple copying invalidates this, this
dnl    is noted in the OS X ranlib man page as a bug, so may not always be
dnl    true). Note LT_INIT claims to obsolete this:
dnl AC_PROG_RANLIB
dnl    so commented out.   

dnl    Documentation.
STAR_LATEX_DOCUMENTATION([sun55])

dnl    Library/component sub-directories. IDL requires special handling.
AC_CONFIG_SUBDIRS([idl])

dnl    Check if we have any IRAF libraries on this platform.
dnl    On success this section gives a -DHAVE_IRAF_LIBIMFORT_A=1 macro
dnl    definition. We cd into iraf, rather than perform this test in the
dnl    configure script in iraf, as this configure script is ran first, so
dnl    cannot depend on the results of a later configure.
(
cd iraf
IRAFSUPPORT="libcompat.a libimfort.a libos.a libsys.a libvops.a"
for f in $IRAFSUPPORT; do
  if test -f $f; then
    rm -f $f
  fi
done
STAR_PLATFORM_SOURCES([$IRAFSUPPORT], 
     [_pc_linux_gnu _sun_solaris _dec_osf _mips _apple_darwin NONE])
for f in $IRAFSUPPORT; do
  if test -f $f; then
    $RANLIB $f
  fi
done
)

dnl    Define -DHAVE_IRAF_LIBIMFORT_A=1, if libimfort.a exists and we can link
dnl    against it. We need to work harder as the presence of platform
dnl    libraries doesn't mean we can link against them (difference compilers
dnl    g77-v-g95 or 64-v-32 bit flags for instance).

AC_MSG_CHECKING([if iraf libraries can create programs])
irafgood=no
if test -f "iraf/libimfort.a"; then
   iraf_save_LIBS=$LIBS
   LIBS="`./iraf_link`"
   AC_LANG_PUSH([Fortran])
   AC_LINK_IFELSE([AC_LANG_CALL([],[imopen])],[irafgood=yes],[irafgood=no])
   AC_LANG_POP([Fortran])
   if test "$irafgood" = yes; then
      AC_DEFINE(HAVE_IRAF_LIBIMFORT_A,1,[Have iraf support])
   fi
   LIBS=$iraf_save_LIBS
fi
AC_MSG_RESULT([$irafgood])
AM_CONDITIONAL(HAVE_WORKING_IRAF_IMFORT, test x$irafgood = xyes)

dnl    Old test.
dnl AC_CHECK_FILES([iraf/libimfort.a])

dnl    Configure .in files.
AC_CONFIG_FILES([Makefile component.xml iraf/Makefile
                 libcof/Makefile libcoi/Makefile libcon/Makefile])

AC_OUTPUT
