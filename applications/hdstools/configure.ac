dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(hdstools, 1.0-2, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([hhelp.f])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_CC

dnl    Setup for creating a monolith.
STAR_MONOLITHS

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [hds fio grp mers par prm sae shl ifd psx])
STAR_DECLARE_DEPENDENCIES(sourceset, sst)

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    Need IFD commands to create package files.
STAR_CHECK_PROGS(ifd2star)

dnl    Need prohlp to create help.
STAR_CHECK_PROGS(prohlp, sst)

dnl Latex/htx documentation
STAR_LATEX_DOCUMENTATION(sun245)

dnl    Configure files.
AC_CONFIG_FILES([Makefile component.xml hdstools.ifd])

dnl    And the fat lady sings.
AC_OUTPUT
