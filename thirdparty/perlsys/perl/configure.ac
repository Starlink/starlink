dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 19893 $)

dnl    Initialisation: package name and version number
AC_INIT(perl, 5.18.2, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([perl/configure.gnu])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    We may want to invoke make recursively
AC_PROG_MAKE_SET
AC_PROG_LN_S

dnl    To configure Perl, run configure.gnu in the subdirectory.
dnl    Use configure.gnu to pick up CC & CFLAGS. Note we add
dnl    /Perl to the prefix, rather than have that as part of the
dnl    prefix (which is surprising) as we want to get a manifest
dnl    file installed into the standard part of the tree and
dnl    work in the standard tree to establish softlinks. This
dnl    latter requirement is the stronger (could use STAR_MANIFEST_DIR 
dnl    to get the manifest file relocated).
dnl
dnl    Export the various FLAGS values to support config.site definitions
dnl    which are not exported. Hack: note that a problem in Perl requires that
dnl    we add CFLAGS to lddlflags, but that's not directly possible, so we
dnl    push it through as part of optimize.
(
   test -n "$CFLAGS" && export CFLAGS
   test -n "$CC" && export CC
   test -n "$LDFLAGS" && export LDFLAGS

   args="-g -O2"
   if test -n "$CFLAGS"; then
      args="$CFLAGS"
   fi

   cc=""
   if test -n "$CC"; then
      cc="-Dcc=$CC"
   fi

   ldflags=
   if test -n "$LDFLAGS"; then
      ldflags="\"-Dldflags=$LDFLAGS\""
   fi

   cd perl; \
   rm -f config.sh ; \
   eval "sh ./Configure -des -Dprefix=${prefix}/Perl $cc $ldflags -Doptimize=\"$args\""
)
 
dnl    Install Perl using a staging area to get all files listed in 
dnl    in the manifest. Also install "starperl" script.
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd perl; $(MAKE) DESTDIR=$$DESTDIR install ); \
   $(MAKE) install-starperl
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
