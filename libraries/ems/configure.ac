dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(ems, 2.4-0, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([EMS_PAR])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_C_CONST
AC_HEADER_STDC
AC_PROG_LIBTOOL

dnl    If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

dnl    Conditional defining whether we build with POSIX thread support.
dnl    We do this by default.
AC_ARG_WITH([pthreads],
            [ --with-pthreads   Build package with POSIX threads support],
            [if test "$withval" = "yes"; then
               use_pthreads="yes"
             else
               use_pthreads="no"
             fi],
            [use_pthreads="yes"])
if test "$use_pthreads" = "yes"; then
   AC_CHECK_LIB([pthread], [pthread_create], ,[use_pthreads="no"])
   if test "$use_pthreads" = "yes"; then
      AC_DEFINE([USE_PTHREADS], [1], [Build with POSIX threads support])
      AC_DEFINE([_REENTRANT], [1], [Make reentrant functions available])
   fi
fi

dnl    Check for reentrant versions of functions.
AC_FUNC_STRERROR_R
AC_CHECK_FUNCS([strtok_r])

dnl    The function ems_fioer.c provides a mapping from Fortran IOSTAT
dnl    codes to error messages.  The only way we can do this is by
dnl    determining a Fortran compiler now, and hoping that is the same
dnl    compiler used to compile the program which actually calls
dnl    ems_fioer.  The configuration in ems_fioer.c.in uses the
dnl    following configure variables.
AC_PROG_FC
AC_SUBST(ac_compiler_gnu)
dnl    Autoconf complains that AC_CANONICAL_BUILD is called multiple
dnl    times.  I don't know where else it's called, but this line does
dnl    appear to be redundant.
dnl AC_CANONICAL_BUILD

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae cnf starmem])
STAR_DECLARE_DEPENDENCIES([link],  [cnf starmem])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(ssn4)

STAR_MESSGEN(ems_err.msg)

AC_CONFIG_HEADERS(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml ems_fioer.c])
AC_CONFIG_FILES([ems_link ems_link_adam])

dnl    This is the bit that does the actual work
AC_OUTPUT
