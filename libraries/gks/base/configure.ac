dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    Note that for GKS the RAL version number if 1.37 but
dnl    Starlink does not increment that version. Rather than use
dnl    non-standard nomenclature use the patch level for starlink patching.
AC_INIT(gks-base, 1.37-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([gkerr.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

dnl    Terminal control
AC_HEADER_STDC
AC_CHECK_HEADERS( termio.h sgtty.h sys/ioctl.h )
AC_CHECK_FUNCS([isatty ioctl])

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS
AC_FC_OPEN_SPECIFIERS(readonly)

dnl    We need to work out how to get the date and time
AC_FC_CHECK_INTRINSICS([CLOCK ITIME IDATE DATE])

dnl    How to bypass Fortran I/O
AC_FC_CHECK_INTRINSICS([FGETC FPUTC FLUSH])

dnl    How to get the environment and process ID
AC_FC_CHECK_INTRINSICS([GETENV GETPID])

dnl    How to manipulate bits
AC_FC_CHECK_INTRINSICS([LSHIFT SHIFTL RSHIFT SHIFTR ISHFT])
AC_FC_CHECK_INTRINSICS([AND IAND OR IOR])

dnl    How to work out where the last non blank character is
AC_FC_CHECK_INTRINSICS([ LNBLNK LEN_TRIM ])

dnl    How to translate a Fortran unit to a Unix file descriptor
AC_FC_CHECK_INTRINSICS([GETFD GETFILEP ISATTY])

dnl    Use config.h
AM_CONFIG_HEADER(config.h)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile gkioop.f])

dnl    This is the bit that does the actual work
AC_OUTPUT
