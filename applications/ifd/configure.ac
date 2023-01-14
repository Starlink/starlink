dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)

AC_INIT(ifd, 1.2-8, starlink@jiscmail.ac.uk)
AC_CONFIG_AUX_DIR([build-aux])
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)
AC_CONFIG_SRCDIR([ifd2star.in])
STAR_DEFAULTS

STAR_LATEX_DOCUMENTATION(ssn68)

dnl    Locate the tclsh binary.
dnl    Do not care which version of tclsh, but since we're Starlink
dnl    may as well use that.
STAR_PATH_TCLTK(8.6, [tcl])

dnl    But depend on Starlink Tcl to make sure one is always available
dnl    during the configure stage.
STAR_DECLARE_DEPENDENCIES(build, [tcl])
STAR_DECLARE_DEPENDENCIES(configure, [tcl])

AC_CONFIG_FILES([Makefile component.xml])

dnl    Make substitutions in some of the script files. 
dnl    The ones which require maketime-sensitive substitutions are handled
dnl    by the Makefile.
AC_CONFIG_FILES([ifd2irafifl],[chmod +x ifd2irafifl])
AC_CONFIG_FILES([ifd_irafhlpgen],[chmod +x ifd_irafhlpgen])
AC_CONFIG_FILES([ifl2ifd],[chmod +x ifl2ifd])
AC_CONFIG_FILES([ifd2iraf],[chmod +x ifd2iraf])
AC_CONFIG_FILES([ifd2star],[chmod +x ifd2star])

AC_OUTPUT
