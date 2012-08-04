dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(graphpar, 1.1-6, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sun113.tex])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_FC
AC_PROG_LIBTOOL
STAR_MONOLITHS

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
dnl    The dependencies should expand to include each of the
dnl    subpackages as they are added to the system
STAR_DECLARE_DEPENDENCIES([build], [sae chr ems par mers starx gks gns pgp sgs])
dnl    There are no `link' or `use' dependencies, because there is no way this
dnl    package can be `used' -- it's just a wrapper

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun113)

AC_CONFIG_FILES([Makefile component.xml
    gkspar/Makefile pgppar/Makefile sgspar/Makefile])

AC_OUTPUT
