dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(smurf, 0.3.2, t.jenness@jach.hawaii.edu)
PACKAGE_UPCASE='SMURF'
AC_DEFINE( [PACKAGE_UPCASE], "SMURF", [Upper cased package name] )
AC_SUBST(PACKAGE_UPCASE)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

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
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CHECK_PROGS( ifd2star )
STAR_CHECK_PROGS( hlib )
STAR_CHECK_PROGS(prohlp,sst)
STAR_CHECK_PROGS(perl)

dnl    Variable sizes - needed for specwriter
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(int)

dnl    Library functions - needed for specwriter
AC_CHECK_FUNCS([mkstemp mkstemps])

dnl    Check for GCC, so we can add SMURF compiler warning flags. 
dnl    Would like to include -fstack-protector, but not all GCC versions 
dnl    support that, so need to check that. 
SMURF_CFLAGS="-Wall -Wextra -Wcast-qual -Wcast-align -Wwrite-strings \
-Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations \
-Wredundant-decls"

if test "$GCC" = "yes"; then
    have_fsp=yes
    AC_MSG_CHECKING([whether ${CC} accepts -fstack-protector])
    fsp_old_cflags="$CFLAGS"
dnl  Assume that any gcc that can take stack-protector also knows about c99
    CFLAGS="$CFLAGS -fstack-protector -std=c99 -pedantic-errors"
    AC_TRY_COMPILE(,,,[have_fsp=no])
    echo $have_fsp
    if test "$have_fsp" = "no"; then
        CFLAGS="$fsp_old_cflags $SMURF_CFLAGS"
    else
        CFLAGS="$CFLAGS $SMURF_CFLAGS"
    fi
fi

dnl AM_CONDITIONAL([HAVE_GCC], [test "$GCC" = "yes"])

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

AC_CHECK_HEADERS(math.h)
AC_CHECK_HEADERS(slamac.h)

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [ast sae pcs mers ndf shl gsl ndg fftw ard kaplibs pda gsd])
STAR_DECLARE_DEPENDENCIES([link], [ndf ast hlp ndg shl pda gsd])
dnl We use the sst package to build documentation (prohlp)
STAR_DECLARE_DEPENDENCIES([sourceset], [sst])

dnl    No use dependencies

dnl Documentation
dnl STAR_LATEX_DOCUMENTATION( sun216/ ssn72/ sc10/ sc11/ )
STAR_LATEX_DOCUMENTATION(sun259)

dnl Ask for a config.h so that we can retain control of when to define
dnl parameters. And also to simplify the build line
AC_CONFIG_HEADERS(config.h)

AC_CONFIG_FILES(Makefile smurf.ifd component.xml libsmurf/Makefile \
                libsmf/Makefile libsc2sim/Makefile libacsis/Makefile \
                libaztec/Makefile libgsd/Makefile libsc2fts/Makefile)
AC_CONFIG_FILES([libsmf/make_h], [chmod 755 libsmf/make_h])


AC_OUTPUT
