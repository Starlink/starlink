dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Configure file for the PCS system.  This is not a component, but
dnl    merely a convenient parent of the directories containing the various
dnl    PCS libraries and their documentation.

AC_INIT(pcs, 4.1-1, ussc@star.rl.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl dtask is a directory rather than a file
AC_CONFIG_SRCDIR([dtask])

STAR_DEFAULTS

dnl    No dependencies, and all the documentation is in pcs-docs

dnl    List the set of active subdirectories.
dnl    This must be kept up-to-date by hand.
dnl    The list is available as the substitution variable @subdirs@.
dnl    (must be kept on one line)
AC_CONFIG_SUBDIRS([adam ams atimer dtask hdspar lex messys misc msp parsecon pcs-docs sock string subpar task])
dnl     Missing from above list:
dnl         adamnet -- redundant, and may not work

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
