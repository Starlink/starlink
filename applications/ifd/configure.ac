dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(ifd, 1.2-8, ussc@star.rl.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR([ifd2star.in])
STAR_DEFAULTS

STAR_LATEX_DOCUMENTATION(ssn68)

dnl    Locate the tclsh binary (should we use STAR_CHECK_PROGS instead?)
AC_ARG_VAR([TCLSH],[TCL 8.2 shell])
AC_PATH_PROG([TCLSH],[tclsh8.2])

AC_CONFIG_FILES([Makefile component.xml])

dnl    Make substitutions in some of the script files. 
dnl    The ones which require maketime-sensitive substitutions are handled
dnl    by the Makefile.
AC_CONFIG_FILES([ifd2irafifl],[chmod +x ifd2irafifl])
AC_CONFIG_FILES([ifd_irafhlpgen],[chmod +x ifd_irafhlpgen])
AC_CONFIG_FILES([ifl2ifd],[chmod +x ifl2ifd])

AC_OUTPUT
