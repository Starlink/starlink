dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
AC_INIT(kaplibs, 2.7-3, ussc@star.rl.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([kaplibs_test.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_FC
AC_PROG_LIBTOOL

dnl    Look for tcl/tk system
STAR_PATH_TCLTK(8.2, [tk])

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_MONOLITHS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [cnf sae mers subpar gks gns idi agi ast cat ndg hds chr fio grp ndf prm par psx tcl tk snx])
STAR_DECLARE_DEPENDENCIES(link, [cat mers subpar psx ndg ndf ast agi fio grp trn prm mag psx pda pgplot tcl tk snx])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun238)

dnl    List the set of active subdirectories.
dnl    This must be kept up-to-date by hand.
dnl    The list is available as the substitution variable @subdirs@.
dnl    (must be kept on one line)
AC_CONFIG_SUBDIRS(aif fts ira irq ctg lpg kpg kgs kapgrf)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])
AC_CONFIG_FILES([kaplibs_link_adam],[chmod +x kaplibs_link_adam])

dnl    This is the bit that does the actual work
AC_OUTPUT
