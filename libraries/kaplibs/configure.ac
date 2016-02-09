dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
AC_INIT([kaplibs],[3.6],[starlink@jiscmail.ac.uk])
AC_CONFIG_AUX_DIR([build-aux])
AC_PREREQ([2.69])
AM_INIT_AUTOMAKE(1.8.2-starlink)

dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([kaplibs_test.f])

dnl    Include defaults for Starlink configurations
STAR_DEFAULTS

dnl    Find required versions of the programs we need for configuration
AC_PROG_CC
AC_PROG_FC
LT_INIT

STAR_CHECK_PROGS([fgeneric])

dnl    Other options here, such as
STAR_CNF_COMPATIBLE_SYMBOLS
STAR_PRM_COMPATIBLE_SYMBOLS
STAR_MONOLITHS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [agi ast atl cat chr cnf fio generic gks gns grp gsl hds idi lpg mers ndf ndg par pcs prm psx sae snx trn])
STAR_DECLARE_DEPENDENCIES(link, [agi ast atl cat ctg fio grp gsl irq lpg mers ndf ndg pcs pda pgplot prm psx psx snx trn])
STAR_DECLARE_DEPENDENCIES(sourceset, [htx sst])

dnl    There are numerous messgen files, and all should be listed here, so
dnl    that they are listed as `predist' files.
STAR_MESSGEN([ kpg/kpg_err.msg  ])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
STAR_LATEX_DOCUMENTATION(sun238)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.  List of subdirectories must match SUBDIRS in Makefile.am
AC_CONFIG_FILES([Makefile component.xml
    aif/Makefile  ira/Makefile     kgs/Makefile
    kpg/Makefile  fts/Makefile  kapgrf/Makefile
    ccg/Makefile])

dnl    This is the bit that does the actual work
AC_OUTPUT
