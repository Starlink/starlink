dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(hds, 5.1-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([hdsState.c])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Define _POSIX_C_SOURCE to be used in all routines. This was
dnl    declared in the old header file "hds1_feature.h", but we need
dnl    to make sure all feature-level defines are included in the autoconf
dnl    tests (this is important for large file support under Solaris, which
dnl    does not define _LARGEFILE_SOURCE without _POSIX_C_SOURCE).
AC_DEFINE([_POSIX_C_SOURCE], [199506L], [Specify the version of the POSIX
standard we want to work with (should provide POSIX.1, POSIX.2 and POSIX.4, 
if available)]) 

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

#   If --with-pic=no is set we should honour that.
AM_CONDITIONAL(NOPIC, test x$pic_mode = xno)

AC_C_BIGENDIAN

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

dnl    Used for dynamic header files
AC_CHECK_HEADERS( time.h )

dnl    Or file locking.
AC_CHECK_FUNCS(fcntl)

dnl    Or fsync
AC_CHECK_FUNCS(fsync)

dnl    RTL "fixups"
AC_CHECK_FUNCS(atexit)
AC_CHECK_FUNCS(on_exit)
AC_CHECK_FUNCS(memmove)

AC_CHECK_FUNCS([getwd getcwd])

dnl    Look for standard headers rather than assuming availability
dnl    by operating system
AC_HEADER_STDC
AC_CHECK_HEADERS(stddef.h)
AC_CHECK_HEADERS(unistd.h)
AC_CHECK_HEADERS([sys/param.h])
AC_CHECK_HEADERS(inttypes.h)

dnl    Look for missing declarations
AC_CHECK_DECLS(confstr)
AC_CHECK_DECLS(getwd)

dnl    fsync is only declared if _POSIX_C_SOURCE isn't!!
AC_CHECK_DECLS(fsync)

dnl    On Darwin access to the _environ symbol is only available to
dnl    the linker when building applications, so cannot be used reliably in
dnl    shared libraries, so need to use the _NSGetEnviron() function.
AC_CHECK_HEADERS(crt_externs.h)
AC_CHECK_FUNCS(_NSGetEnviron)

dnl    Check for largefile support (various macros and fseeko).
dnl    Make sure we don't use the cached version for this (can
dnl    be incorrect due to _POSIX_C_SOURCE on Solaris).
$as_unset ac_cv_sys_largefile_source
STAR_LARGEFILE_SUPPORT

dnl    Calculate sizes and types
AC_CHECK_TYPES([long long int,int,long,long long,off_t,int64_t])
AC_CHECK_TYPES([uint64_t, int32_t, uint32_t, long double, signed char])

AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(long double)
AC_CHECK_SIZEOF(off_t)
AC_CHECK_SIZEOF(size_t)

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [sae chr cnf ems starmem])
STAR_DECLARE_DEPENDENCIES([link],  [chr cnf ems starmem])
STAR_DECLARE_DEPENDENCIES([test],  [chr cnf ems prm starmem])

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
