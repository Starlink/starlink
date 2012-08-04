dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
  
dnl    Initialisation: package name and version number
AC_INIT(nbs, 2.5-10, starlink@jiscmail.ac.uk)
  
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
  
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([NBS_PAR])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_LN_S
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([time.h])
AC_CHECK_HEADERS([sys/times.h])
AC_CHECK_HEADERS([sys/types.h])
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_HEADERS([sys/ipc.h])
AC_CHECK_HEADERS([sys/shm.h])
AC_CHECK_HEADERS([descrip.h])
AC_CHECK_HEADERS([secdef.h])
AC_CHECK_HEADERS([ssdef.h])
AC_CHECK_FUNCS([atexit on_exit])
AC_CHECK_FUNCS([memcpy ots\$move3])
AC_CHECK_FUNCS([shmctl])
AC_CHECK_FUNCS([LIB\$WAIT select])
AC_PROG_LIBTOOL

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae ems cnf])
STAR_DECLARE_DEPENDENCIES([link],  [ems cnf chr])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun77)

STAR_MESSGEN(nbs_err.msg)

dnl    Header checks
AM_CONFIG_HEADER(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
