dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(startcl, 1.5-5, ussc@star.rl.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR([tclAdam.c])
STAR_DEFAULTS

AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

AC_CHECK_LIB(jpeg,jpeg_start_compress)

STAR_DECLARE_DEPENDENCIES(build, [pcs ems gwm mers tclsys sae])

STAR_LATEX_DOCUMENTATION(sun186)
STAR_LATEX_DOCUMENTATION(sun219)

dnl    Locate the tclsh and wish binaries 
dnl    (should we use STAR_CHECK_PROGS instead?)
AC_ARG_VAR([TCLSH],[TCL 8.2 shell])
AC_PATH_PROG([TCLSH],[tclsh8.2])
AC_ARG_VAR([WISH],[TCL 8.2 windowing shell])
AC_PATH_PROG([WISH],[wish8.2])

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
