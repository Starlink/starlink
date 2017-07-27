dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
dnl    Note we use the name "convert" not convert-idl, this is a slight of
dnl    hand to use the same per-package-dirs as convert.
AC_INIT([convert],[1.5-4],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR(IDL_Load.c)

dnl    IDL uses a dlopened shareable library.
AC_DISABLE_STATIC
AC_ENABLE_SHARED
LT_INIT([dlopen])

AC_CHECK_HEADERS([limits.h])
AC_CHECK_DECLS([PATH_MAX],,,[#include <limits.h>
                         ])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS

dnl Link against fortran libraries. Don't want the Fortran main
dnl in this, so remove the known libraries that contain it 
dnl (g77/frtbegin and gfortran/gfortranbegin).
AC_PROG_FC
AC_FC_LIBRARY_LDFLAGS
FCLIBS=`echo $FCLIBS | sed 's:\ \-l.*begin\ : :g'`

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [mers cnf sae ndf perl])
STAR_DECLARE_DEPENDENCIES(link, [ndf])

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile])

STAR_CHECK_PROGS(perl)

dnl    This is the bit that does the actual work
AC_OUTPUT
