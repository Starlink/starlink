dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(daophot, 1.3-6, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([daophot.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)
STAR_MONOLITHS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

dnl    Some code requires the G95 flag -fsloppy-char, see if the current
dnl    compiler supports this (could improve this check by including
dnl    some broken code if necessary, see internal reads and writes
dnl    from a text string to real array in sorter.f).
if test "$GCC" = "yes"; then
    have_fsc=yes
    AC_MSG_CHECKING([whether ${FC} accepts -fsloppy-char])
    fsc_old_fcflags="$FCFLAGS"
    FCFLAGS="$FCFLAGS -fsloppy-char"
    AC_LANG_PUSH([Fortran])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([],[
      PRINT *,'Hello World'
])],,[have_fsc=no])
    AC_LANG_POP([Fortran])
    echo $have_fsc
    if test "$have_fsc" = "no"; then
        FCFLAGS="$fsc_old_fcflags"
    fi
fi

STAR_DECLARE_DEPENDENCIES([build], [sae mers pcs ndf psx chr agi gks cnf], [link])
STAR_LATEX_DOCUMENTATION(sun42)

AC_CONFIG_FILES([Makefile component.xml daophot.csh daophot.sh])

AC_OUTPUT
