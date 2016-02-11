dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([cfitsio],[3.38],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([.])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([cfitsio.news])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Copied from CFITSIO configure.in. Find out the suffix used 
dnl    for shareable libraries. Default is ".so".
SHLIB_SUFFIX=".so"
case "`uname -s`" in
    Darwin*)
        SHLIB_SUFFIX=".dylib"
	;;
    HP-UX*)
        SHLIB_SUFFIX=".sl"
	;;
    CYGWIN*)
        SHLIB_SUFFIX=".dll"
	;;
    *)
esac
AC_SUBST(SHLIB_SUFFIX)

dnl    To configure CFITSIO proper we just run ./configure in the
dnl    cfitsio sub-directory. Do not invoke AC_CONFIG_SUBDIRS,
dnl    since that prompts autoreconf to try to reconfigure that directory, and
dnl    automake to assume it's allowed to play there, too.
(
    cd cfitsio
    ./configure --prefix=$prefix
)

dnl    Find required versions of the programs we need for configuration
LT_INIT
AC_PROG_MAKE_SET

dnl    Check for libraries needed for network services. These are added 
dnl    to the standard link script.
AC_SEARCH_LIBS([gethostbyname], [nsl])
AC_SEARCH_LIBS([connect], [socket],,, [-lnsl])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun227)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml cfitsio_link])

dnl    This is the bit that does the actual work
AC_OUTPUT
