dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(echomop, 3.3-7, starlink@jiscmail.ac.uk)

AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

AC_CONFIG_SRCDIR([echomop.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL

AC_FC_LIBRARY_LDFLAGS
AC_FC_MAIN
STAR_CNF_COMPATIBLE_SYMBOLS

AC_PROG_LN_S
AC_SET_MAKE

AC_FC_HAVE_PERCENTLOC
AC_FC_CHECK_INTRINSICS(loc)

STAR_MONOLITHS

# Find program to compile help libraries, and star2html, for doc/Makefile
STAR_CHECK_PROGS(hlib star2html hlink)

STAR_DECLARE_DEPENDENCIES(build, [cnf fio hds hlp mers ndf par pda pgp prm sae sla shl mers])
STAR_DECLARE_DEPENDENCIES(build, kaplibs, link)
STAR_DECLARE_DEPENDENCIES(sourceset, htx)

# Documented in SUN/152.  The source is in doc/, and the listed products
# are built there.  Then links are made from the current directory by
# Makefile.am, before the whole lot are installed as the default action of
# this macro.
STAR_LATEX_DOCUMENTATION(sun152,
                         [sun152.htx_tar sun152.latex_tar echomop_hyper.tar])

AC_CONFIG_FILES([Makefile component.xml doc/Makefile])
AC_OUTPUT
