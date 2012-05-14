dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(pisa, 2.4-8, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([PSA1_PAR])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

dnl    PISA is a couple of monoliths.
STAR_MONOLITHS
STAR_CHECK_PROGS( ifd2star )

dnl    Build dependencies for this package.
STAR_DECLARE_DEPENDENCIES([build], [agi cat chr fio ndf par pda prm sae shl],[link])
STAR_DECLARE_DEPENDENCIES(sourceset, star2html)

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    Documentation.
STAR_LATEX_DOCUMENTATION(sun109)

dnl    Configure files.
AC_CONFIG_FILES([Makefile component.xml pisa.ifd])

dnl    And the fat lady sings.
AC_OUTPUT
