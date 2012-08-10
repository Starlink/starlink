dnl    Process this file with autoconf to produce a configure script

dnl    This is not a package, so the first two arguments are fake.
AC_INIT(star-docs, 1.0, starlink@jiscmail.ac.uk)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Following sanity-check isn't terribly helpful...
AC_CONFIG_SRCDIR(bootstrap)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

AC_PROG_MAKE_SET

dnl    The following list must be kept up-to-date by hand.
dnl    The list is available as the substitution variable @subdirs@.
dnl    (must be kept on one line)
AC_CONFIG_SUBDIRS([003 006 007 008 018 022 023 024 026 027 030 031 032 033 036 037 039 064 067 075 078])

AC_CONFIG_FILES(Makefile)

AC_OUTPUT
