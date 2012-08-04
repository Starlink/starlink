dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(news, 2.1-2, starlink@jiscmail.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake at least
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([news.in])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET
 
STAR_CHECK_PROGS(sh)
STAR_CHECK_PROGS(echo)

dnl    Depends on the Starlink Perl.
STAR_DECLARE_DEPENDENCIES([configure], [perl])
STAR_CHECK_PROGS(env)

dnl    Documentation is in the parent directory
STAR_LATEX_DOCUMENTATION(sun195)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml)
AC_CONFIG_FILES( news, [chmod +x news])
AC_CONFIG_FILES( newsmaint, [chmod +x newsmaint])
AC_CONFIG_FILES( news2html, [chmod +x news2html])

dnl    This is the bit that does the actual work
AC_OUTPUT
