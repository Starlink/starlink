dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(figaro, 5.6-2, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)

dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([figaro.news])

 
dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS(per-package-dirs)

dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET

dnl    Dependences on Starlink packages
STAR_DECLARE_DEPENDENCIES(build, [pda agi grp ndf fio])

dnl     Needs hlib to build the help library.
STAR_CHECK_PROGS(hlib)

dnl    We have a SUN document, so we should probably tell people it's there.
STAR_LATEX_DOCUMENTATION(sun86)
STAR_DOCS_FILES(sun86a.tex sun86_2df.eps sun86_ech1.eps sun86_ech2.eps sun86_ech3.eps sun86_ech4.eps sun86_ech5.eps sun86_hard.eps sun86_icont.eps sun86_igrey.eps sun86_imag2.eps sun86_imag3.eps sun86_imag.eps sun86_spec2.eps sun86_spec.eps)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile component.xml figaro.csh figaro.icl)

dnl    Need to go into sub-directories.
AC_CONFIG_SUBDIRS([appcb appsub dsa dta fda fig fibre fit fitres fla gen gkd graph ich jty main opt par spext sploop tio twoutil unix work util figaro1 figaro2 figaro3 figaro4 figaro5])

dnl    This is the bit that does the actual work
AC_OUTPUT
