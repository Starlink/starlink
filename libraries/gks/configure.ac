dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    Note that for GKS the RAL version number is 1.37 but
dnl    Starlink does not increment that version. Rather than use
dnl    non-standard nomenclature use the patch level for starlink patching.
AC_INIT(gks, 1.37-1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([gks_err.msg])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [cnf gwm])
STAR_DECLARE_DEPENDENCIES(link,  [ems gwm starx])


AC_HEADER_STDC

dnl    Terminal control
AC_CHECK_HEADERS([termios.h termio.h sgtty.h sys/ioctl.h])
AC_CHECK_FUNCS([isatty ioctl])

dnl    Check for X11
AC_PATH_XTRA

dnl    Having no X11 is fatal.
if test "$no_x" = "yes"; then
   AC_MSG_ERROR([No X11 development system found. Cannot continue])
fi

dnl    Fortran features (mostly for ./base/*)
AC_FC_OPEN_SPECIFIERS(readonly)
AC_FC_RECL_UNIT

# Use Fortran for the remaining function tests
AC_LANG_PUSH([Fortran])

AC_FC_CHECK_INTRINSICS(char)
AC_CHECK_FUNCS([char])

dnl    We need to work out how to get the date and time
AC_FC_CHECK_INTRINSICS([CLOCK ITIME IDATE DATE DATE_AND_TIME])
AC_CHECK_FUNCS([CLOCK ITIME IDATE DATE DATE_AND_TIME])

dnl    How to bypass Fortran I/O
AC_FC_CHECK_INTRINSICS([FGETC FPUTC FLUSH])
AC_CHECK_FUNCS([FGETC FPUTC FLUSH])

dnl    How to get the environment and process ID
AC_FC_CHECK_INTRINSICS([GETENV GETPID])
AC_CHECK_FUNCS([GETENV GETPID])

dnl    How to manipulate bits
AC_FC_CHECK_INTRINSICS([LSHIFT SHIFTL RSHIFT SHIFTR ISHFT])
AC_CHECK_FUNCS([LSHIFT SHIFTL RSHIFT SHIFTR ISHFT])

AC_FC_CHECK_INTRINSICS([AND IAND OR IOR])
AC_CHECK_FUNCS([AND IAND OR IOR])

dnl    How to work out where the last non blank character is
AC_FC_CHECK_INTRINSICS([LNBLNK LEN_TRIM])
AC_CHECK_FUNCS([LNBLNK LEN_TRIM])

dnl    How to translate a Fortran unit to a Unix file descriptor
AC_FC_CHECK_INTRINSICS([GETFD GETFILEP ISATTY FNUM])
AC_CHECK_FUNCS([GETFD GETFILEP ISATTY FNUM])

# No more tests to do, but neatness demands we pop Fortran...
AC_LANG_POP([Fortran])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun83)
dnl STAR_DOCS_FILES(sun83-c1.eps sun83-c2.eps sun83-c3.eps sun83-c4.eps sun83-c5.eps)

STAR_MESSGEN(gks_err.msg)

AC_CONFIG_HEADERS(config.h)

dnl    We recurse into subdirectories -- see SUBDIRS in Makefile.am
AC_CONFIG_FILES([Makefile component.xml
    base/Makefile base/gkioop.f call/Makefile
    data/Makefile data/emf/Makefile data/fonts/Makefile data/wdts/Makefile
    include/Makefile include/gkmc.par.F])

dnl    Not all the driver subdirectories are autoconfed, only...
dnl    (using m4 regexps and substitutions is somewhat safer than being
dnl    hostage to the vaguaries of different platforms' seds)
m4_define([driverlist],
          [canon cgmi cgmo mi mo postscript regis tek4010 wiss xwindows])

AC_CONFIG_FILES([drivers/Makefile ]
                m4_bpatsubst(driverlist, [[a-z0-9]+],
                             [drivers/\&/Makefile]))

dnl    Substitute this list into the drivers/Makefile.in, along with a
dnl    generated list of the corresponding libraries
AC_SUBST(DRIVER_SUBDIRS, "driverlist")
AC_SUBST(DRIVER_LIBLIST, "m4_bpatsubst(driverlist, [[a-z0-9]+],
                                       [drivers/\&/libgks_driver_\&.la])")

AC_OUTPUT
