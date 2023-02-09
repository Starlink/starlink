dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([surf],[1.8-1],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([surf_mon.f])

STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
LT_INIT
AC_PROG_LN_S

AC_FC_CHECK_INTRINSICS([IEEE_FLAGS])

dnl    How to use %LOC function, as LOC or %LOC. Use LOC otherwise
dnl    assume %LOC.
AC_FC_HAVE_PERCENTLOC
AC_FC_CHECK_INTRINSICS([LOC])

AC_FC_CHECK_INTRINSICS([SECNDS])

STAR_MONOLITHS
STAR_CHECK_PROGS( ifd2star )
STAR_CHECK_PROGS( perl )
STAR_CHECK_PROGS( hlib )

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [sae prm pcs kaplibs mers ndf gsl ref pgplot pgp graphpar])
STAR_DECLARE_DEPENDENCIES([link], [ndf fio sla hlp pda prm agi pgplot grp ndg gsl ref pgp graphpar])
dnl    No use dependencies

dnl Documentation
STAR_LATEX_DOCUMENTATION( sun216/ ssn72/ sc10/ sc11/ )

dnl Ask for a config.h so that we can retain control of when to define
dnl parameters. And also to simplify the build line
AC_CONFIG_HEADERS(config.h)

dnl  But we also need to define the docs subdirs
DOCS_SUBDIRS="hlp sun216 ssn72 sc10 sc11"
AC_SUBST(DOCS_SUBDIRS)

AC_CONFIG_FILES(Makefile surflib/Makefile sculib/Makefile src/Makefile docs/Makefile docs/hlp/Makefile docs/sun216/Makefile docs/ssn72/Makefile docs/sc10/Makefile docs/sc11/Makefile component.xml surf_set_app_name.f surf.ifd)

AC_OUTPUT
