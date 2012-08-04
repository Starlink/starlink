dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(agi, 2.1-2, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([agi_err.msg])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

dnl    Other options here, such as
dnl STAR_HAVE_FC_OPEN_READONLY
STAR_CNF_COMPATIBLE_SYMBOLS
AC_FC_CHECK_INTRINSICS([SLEEP EXIT])

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [cnf psx gks sae hds par idi])
dnl    agi_link_adam depends on {pgp,sgs}_link_adam, which are in
dnl    graphpar
STAR_DECLARE_DEPENDENCIES(link,  [ref trn sgs pgp pgplot idi hds graphpar])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun48)

dnl    There is a .msg file in this directory, and this adds Makefile
dnl    rules to generate the xxx_ERR, xxx_err.h and fac_nnn_err files
dnl    which correspond to it.
STAR_MESSGEN(agi_err.msg)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml agp/Makefile])

dnl    This is the bit that does the actual work
AC_OUTPUT
