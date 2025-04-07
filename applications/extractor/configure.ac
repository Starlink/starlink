dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

dnl    Initialisation: package name and version number
AC_INIT([extractor],[2.19-5],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE([1.8.2-starlink subdir-objects])

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([extractor.news])

dnl    Include defaults for Starlink configurations
dnl    Option `per-package-dirs' means that executables and help files
dnl    are installed in package-specific directories.
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
LT_INIT

dnl    Need to build an atask, depends on much the same as a full monolith.
STAR_MONOLITHS

dnl    To configure the two components we just run ./configure in the
dnl    extract and sextractor directories. Do not invoke AC_CONFIG_SUBDIRS,
dnl    since that prompts autoreconf to try to reconfigure that directory, and
dnl    automake to assume it's allowed to play there, too.
dnl
dnl    Added -disable-model-fitting so that we can build the basic
dnl    SExtractor without requiring an ATLAS library.
(
    CFLAGS="$CFLAGS -fcommon"
    cd extract
    ./configure --prefix=$prefix --cache-file=config.cache -disable-model-fitting STARLINK=$STARLINK
    cd ..
    cd sextractor
    ./configure --prefix=$prefix --cache-file=config.cache -disable-model-fitting
)

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [ast ndg sae], [link])
STAR_DECLARE_DEPENDENCIES(sourceset, star2html)

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun226)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml extractor.icl])

dnl    This is the bit that does the actual work
AC_OUTPUT
