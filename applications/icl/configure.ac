dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(icl, 3.1-9, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([icl_io.c])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_CC
AC_PROG_YACC

dnl    See AM_PROG_LEX documentation
AM_PROG_LEX
if test "$LEX" != flex; then
  LEX="$SHELL $missing_dir/missing flex"
  AC_SUBST(LEX_OUTPUT_ROOT, lex.yy)
  AC_SUBST(LEXLIB, '')
fi


AC_HEADER_STDC
AC_PROG_LIBTOOL

# icl_io.c
AC_CHECK_FUNCS([atexit on_exit])

# icl_io.c
AC_CHECK_HEADERS([curses.h cursesX.h])

# icl_io.c
dnl    Or ncurses term.h in /usr/include.
AC_CHECK_HEADERS([term.h ncurses/term.h])

# unix.c should be tweaked to not use environ
AC_CHECK_FUNCS([setenv putenv])
AC_CHECK_FUNCS([unsetenv])

# adam.c
AC_CHECK_FUNCS([strsignal psignal sys_siglist])

# General checks
AC_CHECK_HEADERS([sys/time.h sys/utsname.h sys/wait.h signal.h])

# icl.h
AC_CHECK_FUNCS([isexception finite sys_nerr])

# We expect finite to be defined in math.h
AC_CHECK_DECLS([finite],,,[#include <math.h>])

# carith.c - first add libm to search path with easy function
AC_CHECK_LIB([m],[sin])
AC_CHECK_FUNCS([nint rint])

STAR_CNF_COMPATIBLE_SYMBOLS

# Link against fortran libraries
AC_FC_LIBRARY_LDFLAGS

 
dnl    Documentation is in the parent directory
STAR_LATEX_DOCUMENTATION(sg5)

STAR_MESSGEN(uface_err.msg)

AM_CONFIG_HEADER(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
