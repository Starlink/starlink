dnl    Process this file with autoconf to produce a configure script

dnl    This is not a package, so the first two arguments are fake.
AC_INIT(star-docs, 1.0, ussc@star.rl.ac.uk)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Following sanity-check isn't terribly helpful...
AC_CONFIG_SRCDIR(bootstrap)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_MAKE_SET

dnl    The following list must be kept up-to-date by hand.
dnl    The list is available as the substitution variable @subdirs@.
dnl    (must be kept on one line)
AC_CONFIG_SUBDIRS([sc/002 sc/003 sc/004 sc/006 sc/007 sc/009 sc/012 sc/014 sc/015 sc/017 sg/008 sg/009 sg/010 sg/012 sun/248])

AC_CONFIG_FILES(Makefile)

AC_OUTPUT
