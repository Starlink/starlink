dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(hds, 4.3-5, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([hds_state.c])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

#  Test requires fortran link
AC_FC_MAIN

#   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

AC_C_BIGENDIAN
AC_FC_LIBRARY_LDFLAGS

dnl    Check for OS features we would like
AC_FUNC_VFORK

dnl    The AC_FUNC_MMAP test is broken under Cygwin.
case "$host" in
    *cygwin*)
       AC_CHECK_FUNCS(mmap)
    ;;
    *)
       AC_FUNC_MMAP
    ;;
esac

STAR_CNF_COMPATIBLE_SYMBOLS

dnl    MinGW doesn't have this one.
AC_CHECK_HEADERS(sys/wait.h)

dnl    Or file locking.
AC_CHECK_FUNCS(fcntl)

dnl    Or fsync
AC_CHECK_FUNCS(fsync)

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae chr cnf ems])
STAR_DECLARE_DEPENDENCIES([link],  [chr cnf ems])
STAR_DECLARE_DEPENDENCIES([test],  [chr cnf ems prm])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun92)

dnl    There are two .msg files in this directory, so we need to specify
dnl    the names of them explicitly, rather than them being defaulted from
dnl    the package name.
STAR_MESSGEN([cmp_err.msg dat_err.msg])

AM_CONFIG_HEADER(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)

dnl    This is the bit that does the actual work
AC_OUTPUT
