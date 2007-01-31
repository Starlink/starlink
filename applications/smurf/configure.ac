dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT(smurf, 0.2.2, t.jenness@jach.hawaii.edu)
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

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_MONOLITHS
STAR_CHECK_PROGS( ifd2star )
STAR_CHECK_PROGS( hlib )
STAR_CHECK_PROGS(prohlp,sst)
STAR_CHECK_PROGS(perl)


STAR_MESSGEN( [ libsmf/smf_err.msg ] )

dnl Use --with-gsl=/opt/local to append /opt/local to lib and include paths
dnl Allow --with-gsl=path to work
AC_ARG_WITH(gsl,
        [ --with-gsl=path      Path to use for GSL libraries and include files ],
        if test "$withval" = "yes"; then
          gslextrapath=default
        elif test "$withval" = "no"; then
          gslextrapath=
        else
          gslextrapath=$withval
        fi,
        gslextrapath=default)

if test -n "$gslextrapath"; then
        if test "$gslextrapath" != default; then
           CPPFLAGS="$CPPFLAGS -I$gslextrapath/include"
           LIBS="$LIBS -L$gslextrapath/lib"
        fi
fi

dnl Use --with-expat=/opt/local to append /opt/local to lib and include paths
dnl Allow --with-expat=path to work
AC_ARG_WITH(expat,
	[ --with-expat=path    Path to use for Expat libraries and include files ],
	if test "$withval" = "yes"; then
	  epextrapath=default
	elif test "$withval" = "no"; then
	  epextrapath=
	else
	  epextrapath=$withval
	fi,
	epextrapath=default)

if test -n "$epextrapath"; then
	if test "$epextrapath" != default; then
	   CPPFLAGS="$CPPFLAGS -I$epextrapath/include"
	   LIBS="$LIBS -L$epextrapath/lib"
	fi
fi

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
	   LIBS="$LIBS -L$ncextrapath/lib"
	fi
fi



dnl    External libraries
LIBS="$LIBS -L/usr/lib64"
AC_CHECK_LIB( [gslcblas], [cblas_dsdot], [], [
         echo "Error! Couldn't find libgslcblas. Retry with --with-gsl option?"
        exit -1
        ] )
AC_CHECK_LIB( [gsl], [gsl_acosh], [], [
        echo "Error! Couldn't find libgsl. Retry with --with-gsl option?"
        exit -1
	] )
AC_CHECK_LIB( [expat], [XML_SetUserData], [], [
	echo "Error! Couldn't find libexpat. Retry with --with-expat option?"
	exit -1
	] )

dnl    netcdf is optional - only needed for importing AzTEC data files
AC_CHECK_LIB( [netcdf], [free_NC_attr], [], [
        echo "Warning! Couldn't find libnetcdf. impaztec task won't be available."
        ] )



AC_CHECK_HEADERS(math.h)
AC_CHECK_HEADERS(gsl/gsl_math.h)
AC_CHECK_HEADERS(slamac.h)

dnl    Build dependencies for this package.
dnl    Includes: fio, par, sae; 
dnl    links to: chr, mers, fio, psx, task.
STAR_DECLARE_DEPENDENCIES([build], [ast sae pcs mers ndf shl])
STAR_DECLARE_DEPENDENCIES([link], [ndf ast hlp ndg shl])
dnl We use the sst package to build documentation (prohlp)
STAR_DECLARE_DEPENDENCIES([sourceset], [sst])

dnl    No use dependencies

dnl Documentation
dnl STAR_LATEX_DOCUMENTATION( sun216/ ssn72/ sc10/ sc11/ )

dnl Ask for a config.h so that we can retain control of when to define
dnl parameters. And also to simplify the build line
AC_CONFIG_HEADERS(config.h)

AC_CONFIG_FILES(Makefile smurf.ifd component.xml libsmurf/Makefile \
                libsmf/Makefile libsc2sim/Makefile libacsis/Makefile \
                libaztec/Makefile)

AC_OUTPUT
