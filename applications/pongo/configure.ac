dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(pongo, 2.0-3, starlink@jiscmail.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([pongo_mon.f])

 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS(per-package-dirs)
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL
STAR_MONOLITHS

dnl  Check for library symbols
AC_CHECK_FUNCS([atexit on_exit])

dnl  Extenral programs
STAR_CHECK_PROGS([hlib])

dnl dependances on starlink packages
STAR_DECLARE_DEPENDENCIES([build], [agi sla fio pda], [link])

dnl    Should investigate whether we really need this for a non-starlink
dnl    encumbered pgplot
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    We have a SUN document, we should probably tell people its there
STAR_LATEX_DOCUMENTATION(sun137)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml subs/Makefile tasks/Makefile sun137/Makefile ])

dnl    This is the bit that does the actual work
AC_OUTPUT
