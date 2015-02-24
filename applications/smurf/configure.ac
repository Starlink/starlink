dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([smurf],[1.6.1],[scuba2dr@phas.ubc.ca])
AC_CONFIG_AUX_DIR([build-aux])
PACKAGE_UPCASE='SMURF'
AC_DEFINE( [PACKAGE_UPCASE], "SMURF", [Upper cased package name] )
AC_SUBST(PACKAGE_UPCASE)

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE([1.8.2-starlink subdir-objects])

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([smurf_mon.c])

STAR_DEFAULTS(per-package-dirs)

dnl    Would like the version number as an integer
AC_DEFINE_UNQUOTED([PACKAGE_VERSION_INTEGER], $PACKAGE_VERSION_INTEGER,
                    [Integer version number, in the form major*1e6+minor*1e3+release])

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
LT_INIT

STAR_MONOLITHS
STAR_CHECK_PROGS( ifd2star )
STAR_CHECK_PROGS( hlib )
STAR_CHECK_PROGS(prohlp,sst)
STAR_CHECK_PROGS(prolat,sst)
STAR_CHECK_PROGS(perl)

dnl    Variable sizes - needed for specwriter
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(int)

dnl    Library functions - needed for specwriter
AC_CHECK_FUNCS([mkstemp mkstemps])

dnl    SMURF requires C99 features, so unless we have GCC attempt
dnl    to enable that in the compiler.
if test "$GCC" != "yes"; then
   AC_PROG_CC_C99
fi

dnl    Check for GCC, so we can add SMURF compiler warning flags.
dnl    Would like to include -fstack-protector, but not all GCC versions
dnl    support that, so need to check that. SMURF is using C99 features
dnl    so we always ask that they are supported by GCC.
SMURF_CFLAGS="-std=c99 -Wall -Wextra -Wcast-qual -Wcast-align -Wwrite-strings \
-Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations \
-Wredundant-decls"

if test "$GCC" = "yes"; then
    have_fsp=yes
    AC_MSG_CHECKING([whether ${CC} accepts -fstack-protector])
    fsp_old_cflags="$CFLAGS"
    CFLAGS="$CFLAGS -fstack-protector -pedantic-errors"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])],[],[have_fsp=no])
    echo $have_fsp
    if test "$have_fsp" = "no"; then
        CFLAGS="$fsp_old_cflags $SMURF_CFLAGS"
    else
        CFLAGS="$CFLAGS $SMURF_CFLAGS"
    fi
fi

STAR_MESSGEN( [ libsmf/smf_err.msg ] )

dnl Use --with-netcdf=/opt/local to append /opt/local to lib and include paths
dnl Allow --with-netcdf=path to work
AC_ARG_WITH(netcdf,
	[ --with-netcdf=path    Path to use for Netcdf libraries and include files ],
	if test "$withval" = "yes"; then
	  ncextrapath=default
	elif test "$withval" = "no"; then
	  ncextrapath=
	else
	  ncextrapath=$withval
	fi,
	ncextrapath=default)

if test -n "$ncextrapath"; then
	if test "$ncextrapath" != default; then
	   CPPFLAGS="$CPPFLAGS -I$ncextrapath/include"
	   LDFLAGS="$LDFLAGS -L$ncextrapath/lib"
	fi
fi

dnl    netcdf is optional - only needed for importing AzTEC data files
AC_CHECK_LIB( [netcdf], [free_NC_attr], [], [
        echo "Warning! Couldn't find libnetcdf. impaztec task won't be available."
        ] )

dnl    fail if we don't have ability to do memory mapping
AC_CHECK_FUNCS( mmap msync munmap ftruncate, [], [
         echo "Error! Couldn't find mmap, msync, munmap or ftruncate"
        exit -1
        ] )

dnl    Floating point exception handling is not portable
AC_CHECK_FUNCS( feenableexcept )

AC_CHECK_HEADERS(math.h)
AC_CHECK_HEADERS(mach/mach.h)

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae;
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [ast sae pcs mers ndf shl gsl ndg fftw ard kaplibs pda gsd irq sofa pal one cfitsio thr])
STAR_DECLARE_DEPENDENCIES([link], [ndf ast hlp ndg shl pda gsd irq sofa pal one cfitsio thr])
dnl We use the sst package to build documentation (prohlp and prolat)
STAR_DECLARE_DEPENDENCIES([sourceset], [sst])

dnl    No use dependencies

dnl Documentation
STAR_LATEX_DOCUMENTATION(sun258/ sun259/ sc19/ ssn79/)

dnl Ask for a config.h so that we can retain control of when to define
dnl parameters. And also to simplify the build line
AC_CONFIG_HEADERS(config.h)

dnl  But we also need to define the docs subdirs
DOCS_SUBDIRS="sun258 sun259 sc19 ssn79"
AC_SUBST(DOCS_SUBDIRS)

AC_CONFIG_FILES(Makefile smurf.ifd component.xml libsmurf/Makefile \
                libsmf/Makefile libsc2sim/Makefile libacsis/Makefile \
                libaztec/Makefile libgsd/Makefile libsc2fts/Makefile \
                libsmfftsio/Makefile \
                libnanten/Makefile \
                libsupercam/Makefile \
                docs/Makefile docs/sun258/Makefile docs/sun259/Makefile \
                docs/sc19/Makefile docs/ssn79/Makefile defaults/Makefile scripts/Makefile )

AC_CONFIG_FILES([defaults/make_pardocs], [chmod 755 defaults/make_pardocs])
AC_CONFIG_FILES([libsmf/make_h], [chmod 755 libsmf/make_h])

AC_OUTPUT
