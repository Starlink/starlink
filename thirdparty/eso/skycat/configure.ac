dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(skycat, 3.0.1, ussc@star.rl.ac.uk)

dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Declare dependencies for this package
STAR_DECLARE_DEPENDENCIES([build], [tcl tk tkimg blt itcl tclx], link)

dnl    To configure run ./configure in the subdirectory.
dnl    Do not invoke AC_CONFIG_SUBDIRS, since that prompts autoreconf
dnl    to try to reconfigure that directory, and automake to assume 
dnl    it's allowed to play there, too.
(
    for f in skycat/tclutil skycat/astrotcl skycat/cat skycat/rtd skycat/skycat; do
       (cd $f ; \
       echo "$f: ./configure --prefix=$prefix --cache-file=config.cache" ;\
       ./configure --prefix=$prefix --cache-file=config.cache )
    done
)

STAR_SPECIAL_INSTALL_COMMAND([
    for f in skycat/tclutil skycat/astrotcl skycat/cat skycat/rtd skycat/skycat; do
       (cd $$f ; $(MAKE) DESTDIR=$$DESTDIR install )
    done
])

AC_CONFIG_FILES([Makefile component.xml])

AC_OUTPUT
