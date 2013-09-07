dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision: 24276 $)

dnl    Initialisation: package name and version number
AC_INIT(latex2html, 2012.1, starlink@jiscmail.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([latex2html/latex2html.pin])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS([per-package-dirs])

dnl    latex2html does not use any autotools, so need to pass on
dnl    suitable commands.
(
   cmd="cd latex2html && \
        ./configure --prefix=$datadir/latex2html \
                    --with-texpath=$datadir/latex2html \
                    --disable-eps";
   echo $cmd; \
   eval $cmd
)
           
STAR_SPECIAL_INSTALL_COMMAND([
   ( cd latex2html; $(MAKE) DESTDIR=$$DESTDIR install );
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
