dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(parsecon, 1.3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

#    Sanity-check: name a file in the source directory -- if this
#    isn't found then configure will complain
AC_CONFIG_SRCDIR([parsecon_link_adam])

#    Include defaults for Starlink configurations
STAR_DEFAULTS

#    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

STAR_CNF_COMPATIBLE_SYMBOLS
AC_FC_OPEN_SPECIFIERS(readonly)

#    Declare the build and use dependencies for this package.  Because
#    we build the compifl application, we have a build-time dependency
#    on the link-dependencies of the components we link that against,
#    so use the `option' argument below.
STAR_DECLARE_DEPENDENCIES([build], [hds sae])
STAR_DECLARE_DEPENDENCIES([build], [chr ems string subpar], link)
STAR_DECLARE_DEPENDENCIES([link], [chr ems string subpar])

#    Described (if not documented) in SSN/29

#    Err file has non-default name
STAR_MESSGEN(parse_err.msg)

AC_CONFIG_HEADERS(config.h)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
