dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(ems, 2.0-7, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([EMS_PAR])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae cnf])
STAR_DECLARE_DEPENDENCIES([link],  [cnf])

dnl    Platform-specific sources.  The original set of extensions was
dnl    [alpha_OSF1 ix86_Linux sun4_Solaris], which has migrated to
dnl    [_dec_osf _pc_linux _sun_solaris].  There's scope for more
dnl    general configuration based on autoconf tests, but that's for a
dnl    future project....
STAR_PLATFORM_SOURCES([ems_fioer.c],
                      [_dec_osf _pc_linux _sun_solaris])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(ssn4)

dnl    There is a .msg file in this directory, and this adds Makefile
dnl    rules to generate the xxx_ERR, xxx_err.h and fac_nnn_err files
dnl    which correspond to it.
STAR_MESSGEN(ems_err.msg)

AC_CONFIG_HEADERS(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
