dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 24276 $)

dnl    Initialisation: package name and version number
AC_INIT(vtk, 5.0.3, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([vtk-5.0.3.tar.gz])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    VTK and CMake do not use any autotools, so most of this is a
dnl    complete fudge. Configuration consists of unpacking the tarballs
dnl    and running bootstrap in CMake (this also does a basic build).
(
   cmd="tar zxvf cmake-2.4.7.tar.gz";\
   echo $cmd; \
   eval $cmd
   cmd="tar zxvf vtk-5.0.3.tar.gz";\
   echo $cmd; \
   eval $cmd
   cmd="cd cmake-2.4.7 && ./bootstrap";\
   echo $cmd; \
   eval $cmd
)
           
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd VTK; $(MAKE) DESTDIR=$$DESTDIR INSTALL_ROOT=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
