dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(atools, 1.9-15, starlink@jiscmail.ac.uk)
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([astcopy.f])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_CC

dnl    Setup for creating a monolith.
STAR_MONOLITHS

dnl ATOOLS source files depend on include files from the following components
STAR_DECLARE_DEPENDENCIES([build], [ast cnf hds fio grp ifd ndf par sae])

dnl The ATOOLS link script links against the following components
STAR_DECLARE_DEPENDENCIES([build], [kaplibs atl ast shl], [link])

dnl Other dependencies
STAR_DECLARE_DEPENDENCIES(sourceset, sst)

dnl    Need prohlp to create help.
STAR_CHECK_PROGS(prohlp, sst)

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    Need IFD commands to create package files.
STAR_CHECK_PROGS(ifd2star)
dnl    Need Perl commands to create help file.
STAR_CHECK_PROGS(perl)

dnl    Need a version of Tk wish for astshow script.
STAR_CHECK_PROGS(wish)

dnl    Configure files.
AC_CONFIG_FILES([Makefile component.xml atools.ifd])
AC_CONFIG_FILES([make_history_help], [chmod 755 make_history_help])

dnl    And the fat lady sings.
AC_OUTPUT
