dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 19893 $)

dnl    Initialisation: package name and version number
AC_INIT(perl, 5.10, ussc@star.rl.ac.uk)

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
dnl    which are not exported.
(
   test -n "$CFLAGS" && export CFLAGS
   test -n "$CC" && export CC
   test -n "LDFLAGS" && export LDFLAGS
   cd perl; \
   rm -f config.sh ; \
   cmd="./configure.gnu --prefix=${prefix}/Perl -Duserelocatableinc"; \
   echo $cmd; \
   $cmd; \
)
 
dnl    Install Perl using a staging area to get all files listed in 
dnl    in the manifest. Also install "starperl" script.
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd perl; $(MAKE) DESTDIR=$$DESTDIR install ); \
   $(MAKE) install-starperl
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
