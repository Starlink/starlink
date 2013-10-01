dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(icl, 3.1-11, starlink@jiscmail.ac.uk)
 
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

dnl    Replace AC_PROG_YACC, with following so we can fail if a yacc
dnl    is not found (AC_PROG_YACC sets $YACC to yacc on failure).
AC_CHECK_PROGS(YACC, 'bison -y' byacc yacc, :)
if test "$YACC" = ":"; then
  AC_MSG_ERROR([Failed to locate yacc, bison or byacc])
fi

dnl    It seems that ICL simply refuses to work correctly unless we
dnl    use the -l flag for maximum compatibility with the original lex.
dnl    Also ideally only code produced by flex should be distributed.
dnl    Not having a lex is serious, so stop (don't use AM_PROG_LEX test
dnl    that uses the missing script when lex isn't found).
AC_PROG_LEX

if test "$LEX" = ":"; then
  AC_MSG_ERROR([Failed to locate lex or flex])
fi

if test "$LEX" = flex; then
   icl_AM_LFLAGS="-l"
   AC_SUBST([icl_AM_LFLAGS])
fi

if test "$LEX" != flex; then
   AC_MSG_WARN([Do NOT distribute source code created by the '$LEX' command.])
   AC_MSG_WARN([Only code produced by the flex command is recommended])
   AC_MSG_WARN([for distribution.])
fi

AC_HEADER_STDC
AC_PROG_LIBTOOL

# icl_io.c
AC_CHECK_FUNCS([atexit on_exit])

# icl_io.c
AC_CHECK_HEADERS([curses.h cursesX.h curses/curses.h])

# icl_io.c
dnl    Or ncurses term.h in /usr/include.
AC_CHECK_HEADERS([term.h ncurses/term.h])

dnl Check for library, either -lncurses or -lcurses. Prefer ncurses.
AC_CHECK_LIB([ncurses], [initscr],,AC_CHECK_LIB([curses], [initscr]))
AC_CHECK_LIB([tinfo], [putp],,)

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
AC_FC_MAIN

dnl    Need "hlib" command to build the help library.
STAR_CHECK_PROGS(hlib)

 
dnl    Documentation is in the parent directory
STAR_LATEX_DOCUMENTATION(sg5)

STAR_MESSGEN(uface_err.msg)

dnl    Dependencies
STAR_DECLARE_DEPENDENCIES([build],[pcs cnf sae nbs],[link])
STAR_DECLARE_DEPENDENCIES([link],[pcs cnf hlp nbs hds])


AM_CONFIG_HEADER(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
