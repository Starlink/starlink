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
AC_STDC_HEADERS
AC_CHECK_HEADERS(malloc.h)

STAR_CNF_COMPATIBLE_SYMBOLS

TCL_VERSION=

AC_CHECK_LIB(jpeg,jpeg_start_compress)
AC_SEARCH_LIBS(Tcl_CreateCommand,[tcl$TCL_VERSION tcl],,
               AC_MSG_ERROR([libtcl not found]))
AC_SEARCH_LIBS(Tk_MakeWindowExist,[tk$TCL_VERSION tk],,
               AC_MSG_ERROR([libtk not found]))

STAR_DECLARE_DEPENDENCIES(build, [starx pcs ems gwm mers tclsys sae])

STAR_LATEX_DOCUMENTATION(sun186)
STAR_LATEX_DOCUMENTATION(sun219)

dnl    Locate the tclsh and wish binaries 
dnl    (should we use STAR_CHECK_PROGS instead?)
AC_ARG_VAR([TCLSH],[TCL $TCL_VERSION shell])
AC_PATH_PROG([TCLSH],[tclsh$TCL_VERSION],
             AC_MSG_ERROR([tclsh not found]),[$PATH:$STARLINK/bin])
AC_ARG_VAR([WISH],[TCL $TCL_VERSION windowing shell])
AC_PATH_PROG([WISH],[wish$TCL_VERSION],
             AC_MSG_ERROR([wish not found]),[$PATH:$STARLINK/bin])

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
