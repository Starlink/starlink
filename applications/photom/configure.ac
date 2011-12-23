dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(photom, 1.12-2, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([photom_mon.f])

 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS(per-package-dirs)
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_MONOLITHS

STAR_CHECK_PROGS([hlib])
STAR_CHECK_PROGS([ifd2star])

dnl dependances on starlink packages
STAR_DECLARE_DEPENDENCIES([build],[ard fio grp ndf agi sgs shl pgp gns gks],
                          [link])

dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    We have a SUN document, we should probably tell people its there
STAR_LATEX_DOCUMENTATION(sun45)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml photom.ifd
    help/Makefile subs/Makefile tasks/Makefile])

dnl    This is the bit that does the actual work
AC_OUTPUT
