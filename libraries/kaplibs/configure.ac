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

dnl    Other options here, such as
dnl STAR_CNF_COMPATIBLE_SYMBOLS

dnl    Declare the build and use dependencies for this package
STAR_DECLARE_DEPENDENCIES(build, [cnf sae mers subpar gks gns idi agi ast cat ndg hds chr fio grp ndf prm par psx tclsys])
STAR_DECLARE_DEPENDENCIES(link, [cat mers pcs subpar psx ndg ndf ast agi fio grp trn prm mag psx pda pgplot])

dnl    List the sun/ssn/... numbers which document this package and
dnl    which are present as .tex files in this directory.
dnl    !!!!  DOCUMENTATION TO DO !!!!   STAR_LATEX_DOCUMENTATION(sun114)

LATEX_DOCUMENTATION_FILES = sun238.tex 
DOCUMENTATION_PRODUCTS = $(PAPER_DOCUMENTATION) $(HYPER_DOCUMENTATION)
PAPER_DOCUMENTATION = sun238.tex sun238.ps 
HYPER_DOCUMENTATION = sun238.htx_tar 

# Documentation
@PREDIST@$(PAPER_DOCUMENTATION): builddocs addversion
@PREDIST@       ./builddocs

# The contents of the sun211_figures directory is identical to that
# sun210_figures
sun211_figures: sun210_figures
        $(LN_S) sun210_figures sun211_figures



dnl The following files are built by the targets in this makefile.
MAINTAINERCLEANFILES = $(DOCUMENTATION_PRODUCTS)



dnl    List the set of active subdirectories.
dnl    This must be kept up-to-date by hand.
dnl    The list is available as the substitution variable @subdirs@.
dnl    (must be kept on one line)
AC_CONFIG_SUBDIRS(aif ctm fts ira irq kpg kapgrf)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES([Makefile component.xml])

dnl    This is the bit that does the actual work
AC_OUTPUT
