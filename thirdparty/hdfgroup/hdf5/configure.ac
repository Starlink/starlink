dnl    Process this file with autoconf to produce a configure script

dnl    Initialisation: package name and version number
AC_INIT([hdf5],[1.8.13],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([.])

dnl    Require autoconf-2.50 at least
AC_PREREQ([2.69])
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([configure.ac])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    To configure HDF5 proper we just run ./configure in the
dnl    hdf5 sub-directory. Do not invoke AC_CONFIG_SUBDIRS,
dnl    since that prompts autoreconf to try to reconfigure that directory, and
dnl    automake to assume it's allowed to play there, too.
(
    cd hdf5
    ./configure --prefix=$prefix --enable-threadsafe --disable-hl H5_CFLAGS="-fPIC -Wno-error=implicit-function-declaration -Wno-error=incompatible-function-pointer-types"
)

dnl    Find required versions of the programs we need for configuration
LT_INIT
AC_PROG_MAKE_SET

dnl    Install using a staging area to get all files listed in
dnl    in the manifest. Also install "starperl" script.
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd hdf5; $(MAKE) DESTDIR=$$DESTDIR install );
])

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
