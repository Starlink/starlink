dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(sla, 2.5-7, starlink@jiscmail.ac.uk)
# Version-info specifications.  See SSN/78 for guidelines, and update the table
# below for ANY change of version number.
#
# The library version numbers below match PTW's LIB_VERS 1.6 and 1.7
# respectively, as it happens.  There is no need to continue this pattern
# with any future changes, since these should respect the rather different
# rules for the -version-info numbers.  Instead the PTW makefile LIB_VERS
# changes should be regarded as guidelines for which changes are and are
# not backwards-compatible.
#
#   Release    libsla.la
#    2.4-12       6:0:0
#    2.5-2        7:0:0
AC_SUBST(libsla_la_version_info, 7:0:0)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require automake-1.8.2-starlink at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sla_link])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

dnl    If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

dnl    Platform-dependent/preprocessed sources.  This is slightly
dnl    subtle: file random.F is a preprocessable file.  However,
dnl    there are also versions available for VAX/VMS
dnl    (random.F__vms) and Microsoft Fortran (random.F__win), and
dnl    these are sufficiently distinct that it's not worth just
dnl    configuring the function name.
dnl
dnl    The random and gresid VMS and Windows files have a .F
dnl    extension: there's no preprocessable code in them, but they
dnl    have to have the same name as the file which does have.
dnl
dnl    Problem: Is the code in the *__win files specific to Windows
dnl    or to MSFortran?  Since you'd only get MSFortran on Windows, I
dnl    suppose it's the former (or might as well be).
dnl
dnl    The __vms files will never be matched by this macro (will the
dnl    __win files?), since config.guess doesn't cover VMS at all, but
dnl    the following, as well as documenting the relationship, also
dnl    causes the corresponding files to be included in the
dnl    distribution, where they might be of use to someone.
STAR_PLATFORM_SOURCES([random.F gresid.F wait.f],
                      [__vms __win default])

if cmp -s random.F random.Fdefault; then
    # The unix version, to be configured
    found_random=false
    AC_CHECK_FUNCS([rand random], [found_random=true])
    if $found_random; then
        : OK
    else
        AC_LIBOBJ([rtl_random])
    fi
fi

dnl    Conditional defining whether we build the thread-safe C wrappers
AC_ARG_WITH([pthreads],
            [ --with-pthreads   Build package with POSIX threads support],
        if test "$withval" = "yes"; then
           use_pthreads="yes"
        else
           use_pthreads="no"
        fi,
        use_pthreads="no")
if test "$use_pthreads" = "yes"; then
AC_CHECK_LIB([pthread], [pthread_create], ,[use_pthreads="no"])
   if test "$use_pthreads" = "yes"; then
      AC_DEFINE([USE_PTHREADS], [1], [Build with POSIX threads support])
   fi
fi

dnl    Conditional defining whether we use CNF or not
AC_ARG_WITH([cnf],
            [ --with-cnf    Use Starlink CNF library for thread locking],
        if test "$withval" = "yes"; then
           use_cnf="yes"
        else
           use_cnf="no"
        fi,
        use_cnf="yes")
if test "$use_cnf" = "yes"; then
   AC_DEFINE([USE_CNF], [1], [Use Starlink CNF library for thread locking])
fi

STAR_CNF_COMPATIBLE_SYMBOLS

dnl   We need this for the tests
AC_FC_MAIN
AC_FC_LIBRARY_LDFLAGS

#  Perform the check that configures f77.h.in for the return type of REAL
#  Fortran functions. On 64-bit g77 with f2c compatibility this is double
#  not float.
STAR_CNF_F2C_COMPATIBLE

#  Determine type of Fortran character string lengths.
STAR_CNF_TRAIL_TYPE

AC_CONFIG_HEADERS([config.h])

dnl    Declare the build and use dependencies for this package
dnl    There are neither build nor use dependencies

STAR_LATEX_DOCUMENTATION(sun67)

dnl    Declare the build and use dependencies for this package
dnl    NOTE, cnf should be a link dependency rather than a build
dnl    dependency, but there is clearly a bug in starconf somewhere
dbl    because making it a link dependency results in no CNF dependency
dnl    being added to Makefile.dependencies.
STAR_DECLARE_DEPENDENCIES([build],  [cnf])

AC_CONFIG_FILES(Makefile component.xml vers.f veri.f f77.h)

AC_OUTPUT
