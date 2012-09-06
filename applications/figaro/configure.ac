dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(figaro, 5.6-6, starlink@jiscmail.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([figaro.news])

 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS(per-package-dirs)

dnl    Disable shared libraries: this prevents a couple of the figaro
dnl    libraries being installed as shared libraries (since we don't
dnl    really want them to be installed at all it seems a good idea
dnl    to prevent dependencies from lingering after the build).
AC_DISABLE_SHARED

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_FPP
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET
STAR_MONOLITHS

STAR_CNF_COMPATIBLE_SYMBOLS
AC_C_BIGENDIAN
AC_FC_LIBRARY_LDFLAGS
AC_FC_MAIN
AC_FC_RECL_UNIT

AC_FC_OPEN_SPECIFIERS(access='append')
AC_FC_OPEN_SPECIFIERS(position='append')
AC_FC_OPEN_SPECIFIERS(readonly)
AC_FC_OPEN_SPECIFIERS(action='read')

dnl    How to use %LOC function, as LOC or %LOC. Use LOC otherwise
dnl    assume %LOC.
AC_FC_HAVE_PERCENTLOC()
AC_FC_CHECK_INTRINSICS(LOC)

dnl    Check whether we have X'' or ''X notation
AC_FC_HAVE_OLD_TYPELESS_BOZ()
AC_FC_HAVE_TYPELESS_BOZ()
AC_FC_HAVE_BOZ

dnl    We need to know how big the C pointer type is
dnl    so that we can use CNF_PREG
AC_CHECK_SIZEOF(void*)

dnl    Dependences on Starlink packages
STAR_DECLARE_DEPENDENCIES([build], [pda agi grp ndf fio idi shl sla], [link])

dnl     Needs hlib to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    We have a SUN document, so we should probably tell people it's there.
STAR_LATEX_DOCUMENTATION(sun86)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml figaro.csh figaro.icl
    appcb/Makefile   figaro1/Makefile  fla/Makefile    par/Makefile
    appsub/Makefile  figaro2/Makefile  gen/Makefile    spext/Makefile
    data/Makefile    figaro3/Makefile  gkd/Makefile    sploop/Makefile
    dsa/Makefile     figaro4/Makefile  graph/Makefile  tio/Makefile
    dta/Makefile     figaro5/Makefile  ich/Makefile    twoutil/Makefile
    fda/Makefile     fighelp/Makefile  jty/Makefile    unix/Makefile
    fibre/Makefile   fit/Makefile      main/Makefile   util/Makefile
    fig/Makefile     fitres/Makefile   opt/Makefile    work/Makefile
    figaro.sh])

AC_CONFIG_HEADERS(config.h)

dnl    This is the bit that does the actual work
AC_OUTPUT
