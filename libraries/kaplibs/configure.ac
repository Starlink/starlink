dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
AC_INIT(kaplibs, 2.8-1, ussc@star.rl.ac.uk)
AC_PREREQ(2.50)
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([kaplibs_test.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
AC_PROG_LIBTOOL

STAR_CHECK_PROGS([fgeneric])

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_MONOLITHS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [cnf sae mers subpar gks gns idi agi ast cat ndg hds chr fio grp ndf prm par psx snx])
STAR_DECLARE_DEPENDENCIES(link, [cat mers subpar psx ndg ndf ast agi fio grp trn prm mag psx pda pgplot snx])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun238)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.  List of subdirectories must match SUBDIRS in Makefile.am
AC_CONFIG_FILES([Makefile component.xml
    aif/Makefile  ira/Makefile     kgs/Makefile
    ctg/Makefile  irq/Makefile     kpg/Makefile
    fts/Makefile  kapgrf/Makefile  lpg/Makefile])

dnl    This is the bit that does the actual work
AC_OUTPUT
