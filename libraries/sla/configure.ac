dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(sla, 2.4-12, ussc@star.rl.ac.uk)

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
dnl    subtle: file random.fpp is a preprocessable file.  However,
dnl    there are also versions available for VAX/VMS
dnl    (random.fpp__vms) and Microsoft Fortran (random.fpp__win), and
dnl    these are sufficiently distinct that it's not worth just
dnl    configuring the function name.
dnl
dnl    The random and gresid VMS and Windows files have a .fpp
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
STAR_PLATFORM_SOURCES([random.fpp gresid.fpp wait.f],
                      [__vms __win default])

if cmp -s random.fpp random.fppdefault; then
    # The unix version, to be configured
    #AC_LANG_PUSH([Fortran])
    found_random=false
    AC_CHECK_FUNCS([rand random], [found_random=true])
    if $found_random; then
        : OK
    else
        AC_LIBOBJ([rtl_random])
    fi
    #AC_LANG_POP([Fortran])
fi

STAR_CNF_COMPATIBLE_SYMBOLS

AC_CONFIG_HEADERS([config.h])

dnl    Declare the build and use dependencies for this package
dnl    There are neither build nor use dependencies

STAR_LATEX_DOCUMENTATION(sun67)

AC_CONFIG_FILES(Makefile component.xml)

AC_OUTPUT
