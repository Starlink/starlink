dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([cupid],[2.5],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])
PACKAGE_UPCASE='CUPID'
AC_DEFINE( [PACKAGE_UPCASE], "CUPID", [Upper cased package name] )
AC_SUBST(PACKAGE_UPCASE)


dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([findclumps.c])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_CPP
AC_PROG_FC
LT_INIT

dnl    Setup for creating a monolith.
STAR_MONOLITHS

STAR_MESSGEN( [ cupid_err.msg ] )

dnl CUPID source files depend on include files from the following components
STAR_DECLARE_DEPENDENCIES([build], [mers atl cnf cvg ifd irq ndf sae pda thr])

dnl The CUPID link script links against the following components
STAR_DECLARE_DEPENDENCIES([build], [atl kaplibs ast shl thr], [link])

dnl Other dependencies
STAR_DECLARE_DEPENDENCIES(sourceset, sst)

dnl    Need prohlp to create help.
STAR_CHECK_PROGS(prohlp, sst)

dnl    Need prolat to create documentation.
STAR_CHECK_PROGS(prolat, sst)

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    Need IFD commands to create package files.
STAR_CHECK_PROGS(ifd2star)

dnl    Need Perl commands to create help file.
STAR_CHECK_PROGS(perl)

dnl    Configure files.
AC_CONFIG_FILES([Makefile component.xml cupid.ifd cupidsub/Makefile])
AC_CONFIG_FILES([make_history_help], [chmod 755 make_history_help])
AC_CONFIG_FILES([make_quickdes], [chmod 755 make_quickdes])
AC_CONFIG_FILES([make_doc], [chmod 755 make_doc])
AC_CONFIG_FILES([make_h], [chmod 755 make_h])

STAR_LATEX_DOCUMENTATION(sun255)

dnl    And the fat lady sings.
AC_OUTPUT
